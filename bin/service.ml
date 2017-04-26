open Cmdliner

module Store = Databox_irmin_store

let secret =
  let doc = "if unset, retrieve from arbiter" in
  Arg.(value & opt (some string) None & info ~doc  ["s"; "secret"])


let port =
  let doc = "if set, this will overwrite the value from env variable" in
  Arg.(value & opt (some int) None & info ~doc ["p"; "port"])


let setup_log level =
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()


let setup_log =
  Term.(const setup_log $ Logs_cli.level ())


let service secret port () =
  Lwt_main.run @@ Store.main ?secret ?port ()


let () =
  let info = Term.info ~doc:"databox export service" "service" in
  let term = Term.(const service $ secret $ port $ setup_log) in
  match Term.eval (term, info) with
  | `Ok () | `Version | `Help -> exit 0
  | `Error _ -> exit 1
