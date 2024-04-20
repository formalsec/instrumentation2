open I2
open Cmdliner

let main debug taint_summary file output =
  if debug then Logs.set_level (Some Debug);
  match Run.run ?file ~config:taint_summary ~output () with
  | Ok _n -> 0
  | Error err ->
    Format.eprintf "unexpected error: %a@." Result.pp err;
    Result.int_of_error err

let debug =
  let doc = "debug mode" in
  Arg.(value & flag & info [ "debug" ] ~doc)

let taint_summary =
  let doc = "taint summary" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"SUMM" ~doc)

let file =
  let doc = "normalized file" in
  Arg.(value & pos 1 (some non_dir_file) None & info [] ~docv:"FILE" ~doc)

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
  Cmd.v info Term.(const main $ debug $ taint_summary $ file  $ output)

let () = exit @@ Cmd.eval' cmd
