type t =
  { ty : vuln_type
  ; source : string
  ; source_lineno : int
  ; sink : string
  ; sink_lineno : int
  ; body : Vuln_ast.t list
  }

and vuln_type =
  | Cmd_injection
  | Code_injection
  | Path_traversal
  | Proto_pollution

val pp : Format.formatter -> t -> unit

module Parser : sig
  val from_file : string -> t list Result.t
end
