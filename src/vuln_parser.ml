module Json = Yojson.Basic
module Util = Yojson.Basic.Util
open Vuln
open Format
open Syntax.Result

let parse_vuln_type ?file:_ (ty : Json.t) =
  match Util.to_string ty with
  | "command-injection" -> Ok Cmd_injection
  | "code-injection" -> Ok Code_injection
  | "path-traversal" -> Ok Path_traversal
  | "prototype-pollution" -> Ok Proto_pollution
  | _ -> Error (`Unknown_vuln_type (asprintf "%a" Json.pp ty))

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
  let filename = Util.(member "filename" assoc |> to_option to_string) in
  let* ty = Util.member "vuln_type" assoc |> parse_vuln_type ?file in
  let source = Util.member "source" assoc |> Util.to_string in
  let source_lineno = Util.(member "source_lineno" assoc |> to_option to_int) in
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
    (* Can only have one type of continuation at a time *)
    match Util.member "return" assoc with
    | `Null -> (
      match Util.member "sequence" assoc with
      | `Null -> Ok None
      | tree ->
        let+ tree = from_json ?file tree in
        Some (Sequence tree) )
    | tree ->
      let+ tree = from_json ?file tree in
      Some (Return tree)
  in
  Ok
    { filename
    ; ty
    ; source
    ; source_lineno
    ; sink
    ; sink_lineno
    ; tainted_params
    ; params
    ; cont
    }

let from_file fname =
  try
    let json = Json.from_file ~fname fname in
    Logs.debug (fun m -> m "json of %s:@.%a" fname Json.pp json);
    Util.to_list json |> list_map (from_json ~file:fname)
  with Yojson.Json_error msg -> Error (`Malformed_json msg)
