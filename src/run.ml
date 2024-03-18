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

(** [run file config output] creates symbolic tests [file] from [config] *)
let run file config output =
  let* vulns = Vuln_parser.from_file config in
  let+ module_data = OS.File.read (Fpath.v file) in
  List.mapi
    (fun i vuln ->
      let confs = Vuln.unroll vuln in
      List.mapi
        (fun j conf ->
          let file = get_test_name output (i, j) in
          begin
            match write_test ~file module_data conf with
            | Ok () -> ()
            | Error (`Msg msg) -> failwith msg
          end;
          file )
        confs )
    vulns
  |> List.concat
