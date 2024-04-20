include Stdlib.Result

type err =
  [ `Msg of string
  | `Unknown_vuln_type of string
  | `Malformed_json of string
  ]

let pp fmt = function
  | `Msg s -> Format.pp_print_string fmt s
  | `Unknown_vuln_type s ->
    Format.fprintf fmt "Unknown vulnerability type: %s" s
  | `Malformed_json s -> Format.fprintf fmt "Malformed summary: %s" s

let to_code = function
  | `Msg _ -> 1
  | `Unknown_vuln_type _ -> 2
  | `Malformed_json _ -> 3
