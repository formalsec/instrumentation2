open Syntax

type vuln_conf =
  { ty : vuln_type
  ; source : string
  ; source_lineno : int
  ; sink : string
  ; sink_lineno : int
  ; tainted_params : string list
  ; params : (string * param_type) list
  ; return : vuln_conf option
  }

and param_type =
  [ `Any
  | `Number
  | `String
  | `Boolean
  | `Function
  | `Lazy_object
  | `Array of param_type list
  | `Object of (string * param_type) list
  | `Object2 of (param_type * param_type) list
  | `Union of param_type list (* | `Concrete *)
  ]

and vuln_type =
  | Cmd_injection
  | Code_injection
  | Path_traversal
  | Proto_pollution

let fresh_str =
  let id = ref 0 in
  fun () ->
    incr id;
    Format.sprintf "x_%d" !id

(** [unroll_params params] performs type unrolling of union types *)
let rec unroll_params (params : (string * param_type) list) :
  (string * param_type) list list =
  let rec loop wl acc =
    match wl with
    | [] -> acc
    | ((x, ty) as param) :: wl' ->
      let acc' =
        match ty with
        | `Object prps ->
          unroll_params prps >>= fun prps' ->
          acc >>| List.cons (x, `Object prps')
        | `Union tys -> tys >>= fun ty -> acc >>| List.cons (x, ty)
        | _ -> acc >>| List.cons param
      in
      loop wl' acc'
  in
  loop params [ [] ] >>| List.rev

let rec unroll (vuln : vuln_conf) : vuln_conf list =
  let cs = unroll_params vuln.params >>| fun params -> { vuln with params } in
  match vuln.return with
  | None -> cs
  | Some r ->
    unroll r >>= fun conf ->
    cs >>| fun c -> { c with return = Some conf }

module Fmt = struct
  open Format

  let pp_vuln_type fmt = function
    | Cmd_injection -> fprintf fmt "command-injection"
    | Code_injection -> fprintf fmt "code-injection"
    | Path_traversal -> fprintf fmt "path-traversal"
    | Proto_pollution -> fprintf fmt "prototype-pollution"

  let array_iter x f arr =
    List.iteri (fun i v -> f (x ^ string_of_int i, v)) arr

  let pp_array iter pp_v fmt v =
    pp_print_iter
      ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
      iter pp_v fmt v

  let rec pp_param (box : ('a, formatter, unit) format) fmt
    ((x, ty) : string * param_type) =
    let rec pp_p fmt (x, ty) =
      match ty with
      | `Any -> fprintf fmt {|esl_symbolic.any("%s")|} x
      | `Number -> fprintf fmt {|esl_symbolic.number("%s")|} x
      | `String -> fprintf fmt {|esl_symbolic.string("%s")|} x
      | `Boolean -> fprintf fmt {|esl_symbolic.boolean("%s")|} x
      | `Function -> fprintf fmt {|esl_symbolic.function("%s")|} x
      | `Lazy_object -> fprintf fmt {|esl_symbolic.lazy_object()|}
      | `Object props -> fprintf fmt "@[{ %a@ }@]" pp_obj_props props
      | `Array arr -> fprintf fmt "[ %a ]" (pp_array (array_iter x) pp_p) arr
      | `Object2 props ->
        fprintf fmt "{};@.";
        fprintf fmt "%a"
          (pp_print_list
             ~pp_sep:(fun fmt () -> fprintf fmt ";@\n")
             (fun fmt (t1, t2) ->
               let x1 = fresh_str () in
               let x2 = fresh_str () in
               fprintf fmt "%a;@\n" pp_params_as_decl [ (x1, t1); (x2, t2) ];
               fprintf fmt "%s[%s] = %s" x x1 x2 ) )
          props
      | `Union _ -> assert false
    in
    fprintf fmt box x pp_p (x, ty)

  and pp_obj_props fmt props =
    pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt "@\n, ")
      (pp_param "@[<hov 2>%s:@ %a@]")
      fmt props

  and pp_params_as_decl fmt (params : (string * param_type) list) =
    pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt ";@\n")
      (pp_param "@[<hov 2>var %s =@ %a@]")
      fmt params

  let pp_params_as_args fmt (args : (string * 'a) list) =
    let args = args >>| fst in
    pp_print_list
      ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
      pp_print_string fmt args

  let normalize = String.map (fun c -> match c with '.' | ' ' -> '_' | _ -> c)

  let rec pp fmt (vuln : vuln_conf) =
    let fprintf = Format.fprintf in
    fprintf fmt "// Vuln: %a@\n" pp_vuln_type vuln.ty;
    fprintf fmt "%a;@\n" pp_params_as_decl vuln.params;
    match vuln.return with
    | None -> fprintf fmt "%s(%a);" vuln.source pp_params_as_args vuln.params
    | Some r ->
      let source = asprintf "ret_%s" (normalize vuln.source) in
      fprintf fmt "var %s = %s(%a);@\n" source vuln.source pp_params_as_args
        vuln.params;
      pp fmt { r with source = asprintf "%s%s" source r.source }
end

let pp = Fmt.pp

module Parser : sig
  val from_file : string -> vuln_conf list Result.t
end = struct
  module Json = Yojson.Basic
  module Util = Yojson.Basic.Util
  open Format

  let parse_vuln_type ?file (ty : Json.t) : vuln_type Result.t =
    match Util.to_string ty with
    | "command-injection" -> Ok Cmd_injection
    | "code-injection" -> Ok Code_injection
    | "path-traversal" -> Ok Path_traversal
    | "prototype-pollution" -> Ok Proto_pollution
    | _ ->
      Error
        (asprintf {|%a: unknown type "%a"|}
           (pp_print_option pp_print_string)
           file Json.pp ty )

  let parse_param_type ?file (ty : string) : param_type =
    match String.trim ty with
    | "any" -> `Any
    | "number" -> `Number
    | "string" -> `String
    | "bool" | "boolean" -> `Boolean
    | "function" -> `Function
    | "array" -> `Array [ `String ]
    | "object" -> `Object []
    | "object1" -> `Object2 [ (`String, `String) ]
    | "object2" -> `Object2 [ (`String, `Object2 [ (`String, `String) ]) ]
    | "object3" ->
      `Object2
        [ (`String, `Object2 [ (`String, `Object2 [ (`String, `String) ]) ]) ]
    | "lazy_object" -> `Lazy_object
    | x ->
      printf {|%a: unknown argument type "%s"@.|}
        (pp_print_option pp_print_string)
        file x;
      assert false

  let rec parse_param ?file (param : Json.t) : param_type =
    match param with
    | `String ty -> parse_param_type ?file ty
    | `Assoc obj as assoc -> (
      match Util.member "_union" assoc with
      | `Null -> `Object (obj >>| fun (k, v) -> (k, parse_param ?file v))
      | `List tys -> `Union (tys >>| parse_param ?file)
      | _ ->
        (* should not happen *)
        assert false )
    | `List array_ -> `Array (array_ >>| parse_param ?file)
    | _ ->
      printf {|%a: unknown param "%a"|}
        (pp_print_option pp_print_string)
        file Json.pp param;
      assert false

  let rec from_json ?file (assoc : Json.t) : vuln_conf Result.t =
    let* ty = Util.member "vuln_type" assoc |> parse_vuln_type ?file in
    let source = Util.member "source" assoc |> Util.to_string in
    let source_lineno = Util.member "source_lineno" assoc |> Util.to_int in
    let sink = Util.member "sink" assoc |> Util.to_string in
    let sink_lineno = Util.member "sink_lineno" assoc |> Util.to_int in
    let tainted_params =
      Util.member "tainted_params" assoc |> Util.to_list >>| Util.to_string
    in
    let params =
      Util.member "params_types" assoc |> Util.to_assoc >>| fun (k, v) ->
      (k, parse_param ?file v)
    in
    let* return =
      match Util.member "return" assoc with
      | `Null -> Ok None
      | tree -> from_json ?file tree |> Result.map Option.some
    in
    Ok
      { ty
      ; source
      ; source_lineno
      ; sink
      ; sink_lineno
      ; tainted_params
      ; params
      ; return
      }

  let from_file fname : vuln_conf list Result.t =
    let json = Json.from_file ~fname fname in
    Logs.debug (fun m -> m "json of %s:@.%a" fname Json.pp json);
    Util.to_list json |> list_map (from_json ~file:fname)
end
