let ( let* ) = Result.bind
let ( >>| ) l f = List.map f l

let list_map f lst =
  let exception E of string in
  try
    Ok
      (List.map
         (fun x -> match f x with Ok x -> x | Error e -> raise (E e))
         lst )
  with E e -> Error e

type expr = Call of id * args
and id = string

and arg_type =
  [ `Any
  | `Number
  | `String
  | `Boolean
  | `Function
  | `Array of arg_type list
  | `Object of (string * arg_type) list
  | `Union of arg_type list (* | `Concrete *)
  ]

and args = (string * arg_type) list

type t =
  | Var_decl of string * expr option
  | Expr_stmt of expr

let rec pp_arg_type (box : ('a, Format.formatter, unit) format) fmt
  ((x, ty) : string * arg_type) =
  let fprintf = Format.fprintf in
  let pp_p fmt ty =
    match ty with
    | `Any -> fprintf fmt "esl_symbolic.any(\"%s\")" x
    | `Number -> fprintf fmt "esl_symbolic.number(\"%s\")" x
    | `String -> fprintf fmt "esl_symbolic.string(\"%s\")" x
    | `Boolean -> fprintf fmt "esl_symbolic.boolean(\"%s\")" x
    | `Function -> fprintf fmt "esl_symbolic.function(\"%s\")" x
    | `Object props -> fprintf fmt "{@;@[%a@]@\n}" pp_obj_props props
    | `Array arr -> if List.is_empty arr then fprintf fmt "[]" else assert false
    | `Union _ -> assert false
  in
  fprintf fmt box x pp_p ty

and pp_obj_props fmt props =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@\n")
    (pp_arg_type "  @[<v>%s: %a@]")
    fmt props

let pp_args_as_decl fmt (params : (string * arg_type) list) =
  Format.pp_print_list ~pp_sep:Format.pp_print_newline
    (pp_arg_type "@[<v>var %s = %a;@]")
    fmt params

let pp_args fmt (args : (string * 'a) list) =
  let args = args >>| fst in
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
    Format.pp_print_string fmt args

let pp_expr ?assign fmt v =
  let open Format in
  let (Call (id, args)) = v in
  match assign with
  | None -> fprintf fmt "%a@\n%s(%a)" pp_args_as_decl args id pp_args args
  | Some assign ->
    fprintf fmt "%a@\n%s%s(%a)" pp_args_as_decl args assign id pp_args args

let pp fmt v =
  let open Format in
  match v with
  | Var_decl (lval, rval) ->
    pp_print_option
      ~none:(fun fmt () -> fprintf fmt "var %s" lval)
      (pp_expr ~assign:(Format.sprintf "var %s = " lval))
      fmt rval;
    Format.pp_print_string fmt ";"
  | Expr_stmt e -> fprintf fmt "%a;" (pp_expr ?assign:None) e

module Parser : sig
  val parse_body : Yojson.Basic.t -> t list Result.t
end = struct
  module Json = Yojson.Basic
  module Util = Yojson.Basic.Util

  let parse_param_type ?file (ty : string) : arg_type =
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

  let rec parse_param ?file (param : Json.t) : arg_type =
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

  let parse_id (id : Json.t) : id Result.t =
    Ok (Util.member "name" id |> Util.to_string)

  let parse_expr (expr : Json.t) : expr Result.t =
    let ty = Util.member "type" expr |> Util.to_string in
    match ty with
    | "CallExpression" ->
      let* callee = Util.member "callee" expr |> parse_id in
      let args =
        Util.member "arguments" expr |> Util.to_assoc >>| fun (k, v) ->
        (k, parse_param v)
      in
      Ok (Call (callee, args))
    | _ -> Error (Format.sprintf {|Unknown expression type "%s"|} ty)

  let parse_stmt (stmt : Json.t) : t Result.t =
    let ty = Util.member "type" stmt |> Util.to_string in
    match ty with
    | "VariableDeclaration" -> (
      let decl = Util.member "declaration" stmt in
      let id = Util.member "id" decl |> Util.to_string in
      let init = Util.to_option parse_expr (Util.member "init" decl) in
      match init with
      | None -> Ok (Var_decl (id, None))
      | Some init ->
        let* init in
        Ok (Var_decl (id, Some init)) )
    | "ExpressionStatement" ->
      let* e = Util.member "expression" stmt |> parse_expr in
      Ok (Expr_stmt e)
    | _ -> Error (Format.sprintf {|Unknown statement type "%s".|} ty)

  let parse_body (body : Json.t) : t list Result.t =
    match body with `List l -> list_map parse_stmt l | _ -> Error "not a list"
end
