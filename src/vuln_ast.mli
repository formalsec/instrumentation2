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

val pp : Format.formatter -> t -> unit

module Parser : sig
  val parse_body : Yojson.Basic.t -> t list Result.t
end
