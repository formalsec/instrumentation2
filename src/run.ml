open Bos_setup
open Syntax
module Json = Yojson.Basic
module Util = Yojson.Basic.Util

let logs_on_error ~(use : unit -> 'a) (x : 'a Result.t) =
  match x with
  | Ok x' -> x'
  | Error e ->
    Logs.err (fun m -> m "%s" e);
    use ()

let write_test ~file module_data vuln =
  OS.File.with_oc file
    (fun oc (data, vuln) ->
      Format.eprintf "Genrating %a@." Fpath.pp file;
      Ok
        (Format.kasprintf (output_string oc)
           "%s@\nconst esl_symbolic = require('esl_symbolic');@\n%a@." data
           Vuln.pp vuln ) )
    (module_data, vuln)

let main debug file config output =
  if debug then Logs.set_level (Some Debug);
  let ret =
    let* vulns = Vuln.Parser.from_file config in
    let unrolled = vulns >>| Vuln.unroll in
    let*! module_data = OS.File.read @@ Fpath.v file in
    List.iteri
      (fun i lst ->
        List.iteri
          (fun j vuln ->
            let file =
              match output with
              | "-" -> Fpath.v "-"
              | _ -> Format.ksprintf Fpath.v "%s_%d_%d.js" output i j
            in
            let*! _ = write_test ~file module_data vuln in ())
          lst )
      unrolled;
    Format.eprintf "All OK!";
    Ok 0
  in
  logs_on_error ~use:(fun () -> 1) ret
