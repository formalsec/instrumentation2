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
  | Var_decl of string * expr
  | Expr_stmt of expr

let pp _ = assert false

module Parser : sig
  val parse_body : Yojson.Basic.t -> t list Result.t
end = struct
  module Json = Yojson.Basic
  module Util = Yojson.Basic.Util

  (* let parse_param_type ?file (ty : string) : param_type = *)
  (*   match String.trim ty with *)
  (*   | "any" -> `Any *)
  (*   | "number" -> `Number *)
  (*   | "string" -> `String *)
  (*   | "bool" | "boolean" -> `Boolean *)
  (*   | "function" -> `Function *)
  (*   | "array" -> `Array [] *)
  (*   | "object" | "lazy-object" -> *)
  (*     (1* TODO: lazy-object should be a special type? *1) *)
  (*     `Object [] *)
  (*   | x -> *)
  (*     Format.printf "%a: unknown argument type \"%s\"@." *)
  (*       (Format.pp_print_option Format.pp_print_string) *)
  (*       file x; *)
  (*     assert false *)

  (* let rec parse_param ?file (param : Json.t) : param_type = *)
  (*   match param with *)
  (*   | `String ty -> parse_param_type ?file ty *)
  (*   | `Assoc obj as assoc -> ( *)
  (*     match Util.member "_union" assoc with *)
  (*     | `Null -> `Object (obj >>| fun (k, v) -> (k, parse_param ?file v)) *)
  (*     | `List tys -> `Union (tys >>| parse_param ?file) *)
  (*     | _ -> *)
  (*       (1* should not happen *1) *)
  (*       assert false ) *)
  (*   | `List array_ -> `Array (array_ >>| parse_param ?file) *)
  (*   | _ -> *)
  (*     Format.printf "%a: unknown param \"%a\"" *)
  (*       (Format.pp_print_option Format.pp_print_string) *)
  (*       file Json.pp param; *)
  (*     assert false *)

  (* let from_json ?file *)
  (* let tainted_params = *)
  (*   Util.member "tainted_params" assoc |> Util.to_list >>| Util.to_string *)
  (* in *)
  (* let params = *)
  (*   Util.member "params_types" assoc |> Util.to_assoc >>| fun (k, v) -> *)
  (*   (k, parse_param ?file v) *)
  (* in *)
  (* FIXME: Error handling *)
  (* let return = *)
  (*   let o = Util.to_option (from_json ?file) (Util.member "return" assoc) in *)
  (*   Option.bind o (fun ret -> *)
  (*     match ret with *)
  (*     | Ok conf -> Some conf *)
  (*     | Error e -> *)
  (*       Logs.err (fun m -> m "%s" e); *)
  (*       exit 1 ) *)

  let parse_stmt (_stmt : Json.t) : t Result.t = assert false

  let parse_body (body : Json.t) : t list Result.t =
    Util.to_list body |> list_map parse_stmt
end
