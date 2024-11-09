(* TODO: Logs setup should be deleted before release *)
Fmt_tty.setup_std_outputs ();
Logs.set_level (Some Logs.Debug);
Logs.set_reporter (Logs_fmt.reporter ());

Erg.Server.run_server 8080
