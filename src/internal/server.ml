type server_error =
  | Tcp_address_in_use
  | Tcp_socket_error
  | Non_tcp_client

let server_error_to_string err =
  match err with
  | Tcp_socket_error -> "Tcp_socket_error"
  | Tcp_address_in_use -> "Tcp_address_in_use"
  | Non_tcp_client -> "Non_tcp_client"
;;

let rec write_http_response socket resp =
  let socket_out_channel = Unix.out_channel_of_descr socket in
  let response =
    match resp with
    | Ok resp -> Http_response.to_string (add_content_length resp)
    | Error e -> Http_error.to_string e
  in
  output_string socket_out_channel response;
  flush socket_out_channel

and add_content_length (res : Http_response.t) =
  let body = Bytes.of_string res.message_body in
  let body_length = Bytes.length body |> string_of_int in
  Http_response.set_header (Http_header.ContentLength body_length) res
;;

let rec process_http_request handlers req =
  match req with
  | Error e -> Error e
  | Ok req ->
    let uri = Http_request.(req.request_line.request_uri) |> remove_query in
    let req_http_method = req.request_line.http_method in
    let handler = Handlers.find_by_uri handlers uri in
    (match handler with
     | Some h when h.http_method = req_http_method -> Ok (h.handler_func req)
     | _ ->
       Log.warn (fun m ->
         let http_method = Http_method.to_string req_http_method in
         m "No handlers found for URI: %s `%s`. Responding 404" http_method uri);
       Ok produce_404_response)

and produce_404_response =
  let open Http_response in
  { status_line =
      { http_version = Http_notation.http_version
      ; status_code = 404
      ; reason_phrase = "Not Found"
      }
  ; headers = []
  ; message_body = ""
  }

and remove_query uri =
  match String.split_on_char '?' uri with
  | [ uri; _ ] -> uri
  | _ -> uri
;;

let read_data socket =
  let rec aux acc =
    let buffer = Bytes.create 1024 in
    let received_bytes =
      try Unix.read socket buffer 0 (Bytes.length buffer) with
      | Unix.Unix_error (Unix.EAGAIN, _, _) -> 0
      | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) -> 0
      | e ->
        Log.error (fun m ->
          let e = Printexc.to_string e in
          m "Error reading bytes from the TCP socket: %s" e);
        0
    in
    if received_bytes = 0
    then if Bytes.length acc > 0 then Some acc else None
    else (
      (* Create empty buffer *)
      let updated_data = Bytes.create (Bytes.length acc + received_bytes) in
      (* Copy bytes from previous buffer *)
      Bytes.blit acc 0 updated_data 0 (Bytes.length acc);
      (* Copy received bytes *)
      Bytes.blit buffer 0 updated_data (Bytes.length acc) received_bytes;
      (* Read from tcp again *)
      aux updated_data)
  in
  aux Bytes.empty
;;

(* TODO: Don't sure if the Pipelining - 8.1.2.2 is working now *)
(* TODO: TCP socket close on Connection: close header - 8.1.2.1 *)
(* TODO: Use of the 100 (Continue) Status - 8.2.3 *)
(* TODO: Host header handling - 14.23 *)
let rec handle_client socket handlers =
  (match read_data socket with
   | None -> ()
   | Some bytes ->
     bytes
     |> String.of_bytes
     |> Http_request.from_string
     |> process_http_request handlers
     |> write_http_response socket);
  handle_client socket handlers
;;

let accepting_connections = ref true

let create_server_socket port =
  let module U = Unix in
  try
    let socket_address = U.ADDR_INET (U.inet_addr_loopback, port) in
    let socket = U.socket U.PF_INET U.SOCK_STREAM 0 in
    U.setsockopt socket U.SO_REUSEADDR true;
    U.bind socket socket_address;
    U.listen socket 5;
    Ok socket
  with
  | U.Unix_error (U.EADDRINUSE, _, _) -> Error Tcp_address_in_use
  | _ -> Error Tcp_socket_error
;;

let rec accept_connections server_socket threads handlers =
  (* TODO: Timeouts for connections *)
  if not !accepting_connections
  then (
    List.iter Thread.join threads;
    Unix.close server_socket)
  else (
    let client_socket, client_address = Unix.accept server_socket in
    let client_inet_address =
      match client_address with
      | Unix.ADDR_INET (addr, port) -> Ok (addr, port)
      | _ -> Error Non_tcp_client
    in
    match client_inet_address with
    | Ok (addr, port) ->
      Log.debug (fun m ->
        let addr = Unix.string_of_inet_addr addr in
        m "Recieved client connection, address: %s:%d" addr port);
      Unix.set_nonblock client_socket;
      let handle_client = handle_client client_socket in
      let thread = Thread.create handle_client handlers in
      accept_connections server_socket (thread :: threads) handlers
    | Error e ->
      Log.warn (fun m ->
        m "Failed to accept client connection %s" (server_error_to_string e));
      accept_connections server_socket threads handlers)
;;

let run port handlers =
  Log.info (fun m -> m "Run is called");
  let server_socket = create_server_socket port in
  match server_socket with
  | Ok socket ->
    Log.info (fun m -> m "Server listening on port %d" port);
    accept_connections socket [] handlers
  | Error Tcp_address_in_use ->
    Log.error (fun m -> m "Unable to bind TCP socket, port %d already in use" port)
  | Error _ ->
    Log.error (fun m ->
      m "Unknown error occured while creating server socket on port %d" port)
;;

let stop_server () = accepting_connections := false
