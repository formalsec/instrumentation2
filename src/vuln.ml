type vuln_type =
  | Cmd_injection
  | Code_injection
  | Path_traversal
  | Proto_pollution

type param_type =
  | Any
  | Number
  | String
  | Boolean
  | Function
  | Array of param_type list
  | Object of object_type
  | Union of param_type list (* | `Concrete *)

and object_type =
  [ `Lazy
  | `Polluted of int
  | `Normal of (string * param_type) list
  ]

type vuln_conf =
  { ty : vuln_type
  ; source : string
  ; source_lineno : int option
  ; sink : string
  ; sink_lineno : int option
  ; tainted_params : string list
  ; params : (string * param_type) list
  ; cont : cont option
  }

and cont = Return of vuln_conf

let fresh_str =
  let id = ref 0 in
  fun () ->
    incr id;
    Format.sprintf "x_%d" !id

(** [unroll_params params] performs type unrolling of union types *)
let rec unroll_params (params : (string * param_type) list) :
  (string * param_type) list list =
  let open Syntax.List in
  let rec loop wl acc =
    match wl with
    | [] -> acc
    | ((x, ty) as param) :: wl' ->
      let acc' =
        match ty with
        | Object (`Normal prps) ->
          let* prps' = unroll_params prps in
          List.map (List.cons (x, Object (`Normal prps'))) acc
        | Union tys ->
          let* ty = tys in
          List.map (List.cons (x, ty)) acc
        | _ -> List.map (List.cons param) acc
      in
      loop wl' acc'
  in
  List.map List.rev (loop params [ [] ])

let rec unroll (vuln : vuln_conf) : vuln_conf list =
  let open Syntax.List in
  let cs =
    List.map (fun params -> { vuln with params }) (unroll_params vuln.params)
  in
  match vuln.cont with
  | None -> cs
  | Some (Return r) ->
    let* conf = unroll r in
    let+ c = cs in
    { c with cont = Some (Return conf) }

module Fmt = struct
  open Format

  let pp_vuln_type fmt = function
    | Cmd_injection -> fprintf fmt "command-injection"
    | Code_injection -> fprintf fmt "code-injection"
    | Path_traversal -> fprintf fmt "path-traversal"
    | Proto_pollution -> fprintf fmt "prototype-pollution"

  let array_iter x f arr =
    List.iteri (fun i v -> f (x ^ string_of_int i, v)) arr

  let pp_iter ~pp_sep iter pp_v fmt v =
    let is_first = ref true in
    let pp_v v =
      if !is_first then is_first := false else pp_sep fmt ();
      pp_v fmt v
    in
    iter pp_v v

  let pp_array (iter : ('a -> unit) -> 'b -> unit) pp_v fmt v =
    pp_iter ~pp_sep:(fun fmt () -> pp_print_string fmt ", ") iter pp_v fmt v

  let rec pp_param (box : ('a, formatter, unit) format) fmt
    ((x, ty) : string * param_type) =
    let rec pp_p fmt (x, ty) =
      match ty with
      | Any -> fprintf fmt {|esl_symbolic.any("%s")|} x
      | Number -> fprintf fmt {|esl_symbolic.number("%s")|} x
      | String -> fprintf fmt {|esl_symbolic.string("%s")|} x
      | Boolean -> fprintf fmt {|esl_symbolic.boolean("%s")|} x
      | Function -> fprintf fmt {|esl_symbolic.function("%s")|} x
      | Object `Lazy -> fprintf fmt "esl_symbolic.lazy_object()"
      | Object (`Polluted n) -> fprintf fmt "esl_symbolic.polluted_object(%d)" n
      | Object (`Normal props) -> fprintf fmt "@[{ %a@ }@]" pp_obj_props props
      | Array arr -> fprintf fmt "[ %a ]" (pp_array (array_iter x) pp_p) arr
      | Union _ -> assert false
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
      (pp_param "@[<hov 2>let %s =@ %a@]")
      fmt params

  let pp_params_as_args fmt (args : (string * 'a) list) =
    let args = List.map fst args in
    pp_print_list
      ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
      pp_print_string fmt args

  let normalize = String.map (fun c -> match c with '.' | ' ' -> '_' | _ -> c)

  let pp fmt (vuln : vuln_conf) =
    fprintf fmt "// Vuln: %a@\n" pp_vuln_type vuln.ty;
    let rec aux fmt vuln =
      fprintf fmt "%a;@\n" pp_params_as_decl vuln.params;
      match vuln.cont with
      | None -> fprintf fmt "%s(%a);" vuln.source pp_params_as_args vuln.params
      | Some (Return r) ->
        let source = asprintf "ret_%s" (normalize vuln.source) in
        fprintf fmt "var %s = %s(%a);@\n" source vuln.source pp_params_as_args
          vuln.params;
        aux fmt { r with source = asprintf "%s%s" source r.source }
    in
    aux fmt vuln;
    match vuln.ty with
    | Proto_pollution -> fprintf fmt "@\nconsole.log(({}).toString);"
    | _ -> ()
end

let pp = Fmt.pp

module Parser : sig
  val from_file : string -> (vuln_conf list, [> `Msg of string ]) Result.t
end = struct
  module Json = Yojson.Basic
  module Util = Yojson.Basic.Util
  open Format
  open Syntax.Result

  let parse_vuln_type ?file (ty : Json.t) =
    match Util.to_string ty with
    | "command-injection" -> Ok Cmd_injection
    | "code-injection" -> Ok Code_injection
    | "path-traversal" -> Ok Path_traversal
    | "prototype-pollution" -> Ok Proto_pollution
    | _ ->
      Error
        (`Msg
          (asprintf {|%a: unknown type "%a"|}
             (pp_print_option pp_print_string)
             file Json.pp ty ) )

  let parse_param_type ?file (ty : string) : param_type =
    match String.trim ty with
    | "any" -> Any
    | "number" -> Number
    | "string" -> String
    | "bool" | "boolean" -> Boolean
    | "function" -> Function
    | "array" -> Array [ String ]
    | "object" -> Object (`Normal [])
    | "polluted_object2" -> Object (`Polluted 2)
    | "polluted_object3" -> Object (`Polluted 3)
    | "lazy_object" -> Object `Lazy
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
      | `Null ->
        let params = List.map (fun (k, v) -> (k, parse_param ?file v)) obj in
        Object (`Normal params)
      | `List tys -> Union (List.map (parse_param ?file) tys)
      | _ ->
        (* should not happen *)
        assert false )
    | `List array_ -> Array (List.map (parse_param ?file) array_)
    | _ ->
      printf {|%a: unknown param "%a"|}
        (pp_print_option pp_print_string)
        file Json.pp param;
      assert false

  let rec from_json ?file (assoc : Json.t) =
    let* ty = Util.member "vuln_type" assoc |> parse_vuln_type ?file in
    let source = Util.member "source" assoc |> Util.to_string in
    let source_lineno =
      Util.(member "source_lineno" assoc |> to_option to_int)
    in
    let sink = Util.member "sink" assoc |> Util.to_string in
    let sink_lineno = Util.(member "sink_lineno" assoc |> to_option to_int) in
    let tainted_params =
      Util.member "tainted_params" assoc
      |> Util.to_list |> List.map Util.to_string
    in
    let params =
      Util.member "params_types" assoc
      |> Util.to_assoc
      |> List.map (fun (k, v) -> (k, parse_param ?file v))
    in
    let* cont =
      match Util.member "return" assoc with
      | `Null -> Ok None
      | tree ->
        let+ tree = from_json ?file tree in
        Some (Return tree)
    in
    Ok
      { ty
      ; source
      ; source_lineno
      ; sink
      ; sink_lineno
      ; tainted_params
      ; params
      ; cont
      }

  let from_file fname =
    let json = Json.from_file ~fname fname in
    Logs.debug (fun m -> m "json of %s:@.%a" fname Json.pp json);
    Util.to_list json |> list_map (from_json ~file:fname)
end
