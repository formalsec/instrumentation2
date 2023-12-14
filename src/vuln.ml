open Syntax

type vuln_conf =
  { ty : vuln_type
  ; source : string
  ; source_lineno : int
  ; sink : string
  ; sink_lineno : int
  ; tainted_params : string list
  ; params : (string * param_type) list
  }

and param_type =
  [ `Any
  | `Number
  | `String
  | `Boolean
  | `Function
  | `Array of param_type list
  | `Object of (string * param_type) list
  | `Union of param_type list (* | `Concrete *)
  ]

and vuln_type =
  | Cmd_injection
  | Code_injection
  | Path_traversal
  | Proto_pollution

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

let unroll (vuln : vuln_conf) : vuln_conf list =
  unroll_params vuln.params >>| fun params -> { vuln with params }

module Fmt = struct
  open Format

  let pp_vuln_type fmt = function
    | Cmd_injection -> fprintf fmt "command-injection"
    | Code_injection -> fprintf fmt "code-injection"
    | Path_traversal -> fprintf fmt "path-traversal"
    | Proto_pollution -> fprintf fmt "prototype-pollution"

  let rec pp_param (box : ('a, formatter, unit) format) fmt
    ((x, ty) : string * param_type) =
    let fprintf = Format.fprintf in
    let pp_p fmt ty =
      match ty with
      | `Any -> fprintf fmt {|esl_symbolic.any("%s")|} x
      | `Number -> fprintf fmt {|esl_symbolic.number("%s")|} x
      | `String -> fprintf fmt {|esl_symbolic.string("%s")|} x
      | `Boolean -> fprintf fmt {|esl_symbolic.boolean("%s")|} x
      | `Function -> fprintf fmt {|esl_symbolic.function("%s")|} x
      | `Object props -> fprintf fmt "@[{ %a@ }@]" pp_obj_props props
      | `Array arr ->
        if List.is_empty arr then fprintf fmt "[]" else assert false
      | `Union _ -> assert false
    in
    fprintf fmt box x pp_p ty

  and pp_obj_props fmt props =
    pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt "@\n, ")
      (pp_param "@[<hov 2>%s:@ %a@]")
      fmt props

  let pp_params_as_decl fmt (params : (string * param_type) list) =
    pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt ";@\n")
      (pp_param "@[<hov 2>var %s =@ %a@]")
      fmt params

  let pp_params_as_args fmt (args : (string * 'a) list) =
    let args = args >>| fst in
    pp_print_list
      ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
      pp_print_string fmt args

  let pp fmt (vuln : vuln_conf) =
    let fprintf = Format.fprintf in
    fprintf fmt "// Vuln: %a@\n" pp_vuln_type vuln.ty;
    fprintf fmt "%a;@\n" pp_params_as_decl vuln.params;
    fprintf fmt "%s(%a);" vuln.source pp_params_as_args vuln.params
end

let pp = Fmt.pp

module Parser : sig
  val from_file : string -> vuln_conf list Result.t
end = struct
  module Json = Yojson.Basic
  module Util = Yojson.Basic.Util

  let parse_vuln_type ?file (ty : Json.t) : vuln_type Result.t =
    match Util.to_string ty with
    | "command-injection" -> Ok Cmd_injection
    | "code-injection" -> Ok Code_injection
    | "path-traversal" -> Ok Path_traversal
    | "prototype-pollution" -> Ok Proto_pollution
    | _ ->
      Error
        (Format.asprintf "%a: unknown type \"%a\""
           (Format.pp_print_option Format.pp_print_string)
           file Json.pp ty )

  let parse_param_type ?file (ty : string) : param_type =
    match String.trim ty with
    | "any" -> `Any
    | "number" -> `Number
    | "string" -> `String
    | "bool" | "boolean" -> `Boolean
    | "function" -> `Function
    | "array" -> `Array []
    | "object" | "lazy-object" ->
      (* TODO: lazy-object should be a special type? *)
      `Object []
    | x ->
      Format.printf "%a: unknown argument type \"%s\"@."
        (Format.pp_print_option Format.pp_print_string)
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
      Format.printf "%a: unknown param \"%a\""
        (Format.pp_print_option Format.pp_print_string)
        file Json.pp param;
      assert false

  let from_json ?file (assoc : Json.t) : vuln_conf Result.t =
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
    Ok { ty; source; source_lineno; sink; sink_lineno; tainted_params; params }

  let from_file fname : vuln_conf list Result.t =
    let json = Json.from_file ~fname fname in
    Logs.debug (fun m -> m "json of %s:@.%a" fname Json.pp json);
    Util.to_list json |> list_map (from_json ~file:fname)
end
