open Cmdliner
open Instrumentation2

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
    ; `P "Report them in https://turbina.gsd.inesc-id.pt:8080/tiago/js-cpg"
    ]
  in
  let info = Cmd.info "Instrumentor2" ~version:"%%VERSION%%" ~doc ~man in
  Cmd.v info Term.(const Run.main $ debug $ file $ config $ output)

let () = exit @@ Cmd.eval' cmd
