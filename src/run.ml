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
let run ?file ~config ~output () =
  let+ vulns = Vuln_parser.from_file config in
  List.mapi
    (fun i vuln ->
      let confs = Vuln.unroll vuln in
      List.mapi
        (fun j conf ->
          let output_file = get_test_name output (i, j) in
          let filename =
            match file with
            | Some f -> f
            | None ->
              let filename =
                match conf.Vuln.filename with
                | Some f -> f
                | None -> assert false
              in
              Filename.(concat (dirname config) filename)
          in
          let module_data = In_channel.(with_open_text filename input_all) in
          begin
            match write_test ~file:output_file module_data conf with
            | Ok () -> ()
            | Error (`Msg msg) -> failwith msg
          end;
          output_file )
        confs )
    vulns
  |> List.concat
