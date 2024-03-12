open Bos_setup
open Syntax.Result
module Json = Yojson.Basic
module Util = Yojson.Basic.Util

let get_test_name prefix (i, j) =
  match prefix with
  | "-" -> Fpath.v "-"
  | _ -> Format.ksprintf Fpath.v "%s_%d_%d.js" prefix i j

let write_test ~file module_data vuln =
  Format.eprintf "Genrating %a@." Fpath.pp file;
  OS.File.writef file "%s@\n%a@." module_data Vuln.pp vuln

let run file config output =
  let* vulns = Vuln_parser.from_file config in
  let* module_data = OS.File.read (Fpath.v file) in
  List.iteri
    (fun i vuln ->
      let confs = Vuln.unroll vuln in
      List.iteri
        (fun j conf ->
          let file = get_test_name output (i, j) in
          match write_test ~file module_data conf with
          | Ok () -> ()
          | Error (`Msg msg) -> failwith msg )
        confs )
    vulns;
  Ok 0

let main debug file config output =
  if debug then Logs.set_level (Some Debug);
  match run file config output with
  | Ok n -> n
  | Error (`Msg msg) ->
    Format.eprintf "error: %s@." msg;
    1
