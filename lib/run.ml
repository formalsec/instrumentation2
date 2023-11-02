open Bos_setup
module Json = Yojson.Basic
module Util = Yojson.Basic.Util

let ( let*! ) r f = match r with Error (`Msg e) -> failwith e | Ok v -> f v
let ( let* ) = Result.bind
let ( >>| ) lst f = List.map f lst

let list_map f lst =
  let exception E of string in
  try
    Ok
      (List.map
         (fun x -> match f x with Ok x -> x | Error e -> raise (E e))
         lst )
  with E e -> Error e

let logs_on_error ~(use : unit -> 'a) (x : 'a Result.t) =
  match x with
  | Ok x' -> x'
  | Error e ->
    Logs.err (fun m -> m "%s" e);
    use ()

let main debug file config _output =
  if debug then Logs.set_level (Some Debug);
  let ret =
    let open Vuln in
    let* vulns = Vuln.Parser.from_file config in
    let unrolled = vulns >>| Vuln.unroll in
    let*! test_data = OS.File.read @@ Fpath.v file in
    List.iteri
      (fun i lst ->
        List.iteri
          (fun j vuln ->
            let path = Fpath.v @@ Printf.sprintf "symbolic_test_%d_%d.js" i j in
            Format.printf "Genrating %a@." Fpath.pp path;
            let*! () =
              OS.File.writef path
                "const esl_symbolic = require('esl_symbolic')@.%s@.%a" test_data
                Vuln.pp vuln
            in
            () )
          lst )
      unrolled;
    Logs.app (fun m -> m "All OK!");
    Ok 0
  in
  logs_on_error ~use:(fun () -> 1) ret
