(* TODO: Logs setup should be deleted before release *)
Fmt_tty.setup_std_outputs ();
Logs.set_level (Some Logs.Debug);
Logs.set_reporter (Logs_fmt.reporter ());
let handlers = Erg.empty_handlers in
let handler =
  Erg.create_handler Erg.Get "/currencies" (fun req ->
    let query =
      match Erg.query req with
      | None -> "no query"
      | Some q -> q
    in
    let response =
      Printf.sprintf "{\n\t\"message\": \"It's working\";\n\t\"query\": \"%s\";}" query
    in
    Erg.empty_http_response
    |> Erg.set_status_code Erg.OK
    |> Erg.set_header (Erg.ContentType "application/json")
    |> Erg.set_response_body response)
in
let handlers = Erg.add_handler handler handlers in
Erg.start 8080 handlers
