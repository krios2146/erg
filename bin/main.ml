(* TODO: Logs setup should be deleted before release *)
Fmt_tty.setup_std_outputs ();
Logs.set_level (Some Logs.Debug);
Logs.set_reporter (Logs_fmt.reporter ());
let handlers = Erg.empty_handlers () in
let handler =
  Erg.create_handler Erg.Get "/currencies" (fun req ->
    let query =
      match Erg.get_param req "some" with
      | None -> "No some parameter"
      | Some q -> "some parameter is: " ^ q
    in
    let headers =
      Erg.get_headers req
      |> List.map Erg_internal.Http_header.pretty_print
      |> List.map (String.split_on_char ':')
      |> List.map (fun h ->
        let header = List.nth h 1 |> String.trim in
        let value = List.nth h 2 in
        Printf.sprintf "\"%s\": %s;" header value)
      |> List.fold_left (fun acc h -> acc ^ h ^ "\n\t\t") String.empty
    in
    let response =
      Printf.sprintf
        "{\n\
         \t\"message\": \"It's working\";\n\
         \t\"query\": \"%s\";\n\
         \t\"headers\": [%s];\n\
         }"
        query
        headers
    in
    Erg.empty_http_response
    |> Erg.set_status_code Erg.OK
    |> Erg.set_header (Erg.ContentType "application/json")
    |> Erg.set_response_body response)
in
let handlers = Erg.add_handler handler handlers in
Erg.start 8080 handlers
