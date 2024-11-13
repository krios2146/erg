(* TODO: Logs setup should be deleted before release *)
Fmt_tty.setup_std_outputs ();
Logs.set_level (Some Logs.Debug);
Logs.set_reporter (Logs_fmt.reporter ());
let handlers = Erg.empty_handlers in
let handler =
  Erg.create_handler Erg.Get "/currencies" (fun _ ->
    Erg.empty_http_response
    |> Erg.set_status_code Erg.OK
    |> Erg.set_response_body "It's working")
in
let handlers = Erg.add_handler handler handlers in
Erg.start 8080 handlers
