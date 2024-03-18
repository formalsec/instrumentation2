open I2
open Cmdliner

let main debug file config output =
  if debug then Logs.set_level (Some Debug);
  match Run.run file config output with
  | Ok _n -> 0
  | Error (`Msg msg) ->
    Format.eprintf "error: %s@." msg;
    1

let debug =
  let doc = "debug mode" in
  Arg.(value & flag & info [ "debug" ] ~doc)

let file =
  let doc = "normalized file" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"FILE" ~doc)

let config =
  let doc = "taint summary" in
  Arg.(required & pos 1 (some non_dir_file) None & info [] ~docv:"SUMM" ~doc)

let output =
  let doc = "output file" in
  Arg.(value & opt string "symbolic_test" & info [ "output"; "o" ] ~doc)

let cmd =
  let doc = "Instrumentor2" in
  let man =
    [ `S Manpage.s_bugs
    ; `P "Report them in https://github.com/formalsec/instrumentation2/issues"
    ]
  in
  let info = Cmd.info "instrumentation2" ~version:"%%VERSION%%" ~doc ~man in
  Cmd.v info Term.(const main $ debug $ file $ config $ output)

let () = exit @@ Cmd.eval' cmd
