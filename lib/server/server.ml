type error =
  | No_request_line
  | Empty_request_line
  | Incomplete_request_line
  | Malformed_request_line

(* let http_version = "HTTP/1.1" *)
(* let cr = '\r' *)
(* let lf = '\n' *)
(* let ht = '\t' *)
let sp = ' '
let crlf = "\r\n"

(* type http_method = *)
(*   | OPTIONS *)
(*   | GET *)
(*   | HEAD *)
(*   | POST *)
(*   | PUT *)
(*   | DELETE *)
(*   | TRACE *)
(*   | CONNECT *)

let accepting_connections = ref true

let create_server_socket port =
  try
    let module U = Unix in
    let socket_address = U.ADDR_INET (U.inet_addr_loopback, port) in
    let socket = U.socket U.PF_INET U.SOCK_STREAM 0 in

    U.setsockopt socket U.SO_REUSEADDR true;
    U.bind socket socket_address;
    U.listen socket 5;

    Some socket
  with
  | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
      Log.error (fun m ->
          m "Unable to bind TCP socket, port %d already in use" port);
      None
  | _ ->
      Log.error (fun m ->
          m "Unknown error occured while creating server socket on port %d" port);
      None

let find_substring_index s sub =
  let s_len = String.length s in
  let sub_len = String.length sub in
  let rec aux i =
    if i > s_len then None
    else if String.sub s i sub_len = sub then Some i
    else aux (i + 1)
  in
  aux 0

let parse_request_line req_line =
  let req_line_parts = String.split_on_char sp req_line in
  match req_line_parts with
  | [] -> Error Empty_request_line
  | [ _; _ ] -> Error Incomplete_request_line
  | [ http_method; uri; version ] ->
      Ok
        (Printf.sprintf "Method: %s Request-URI: %s HTTP-Version: %s"
           http_method uri version)
  | _ -> Error Malformed_request_line

let handle_tcp_client_data data =
  Log.debug (fun m -> m "Recieved data:\n%s" data);
  let crlf_index = find_substring_index data crlf in
  let request_line =
    match crlf_index with
    | None -> Error No_request_line
    | Some i -> parse_request_line (String.sub data 0 i)
  in
  request_line

let error_to_string err =
  match err with
  | No_request_line -> "No_request_line"
  | Empty_request_line -> "Empty_request_line"
  | Incomplete_request_line -> "Incomplete_request_line"
  | Malformed_request_line -> "Malformed_request_line"

let write_http_response socket data =
  let status_line = "HTTP/1.1 501 Not Implemented \r\n" in
  let entity = data in
  let response = status_line ^ crlf ^ entity in
  let response_bytes = Bytes.of_string response in
  let response_length = Bytes.length response_bytes in
  ignore (Unix.write socket response_bytes 0 response_length)

let handle_client socket =
  let buffer = Bytes.create 1024 in
  let received_bytes = Unix.read socket buffer 0 1024 in

  (if received_bytes > 0 then
     let data = Bytes.sub_string buffer 0 received_bytes in
     let handeled_data = handle_tcp_client_data data in
     match handeled_data with
     | Error e -> write_http_response socket (error_to_string e)
     | Ok data -> write_http_response socket data);
  Unix.close socket

let accept_connection server_socket =
  Log.debug (fun m -> m "Accepting connections");

  (* Not sure whether doing it recursively is a good idea, so there is a loop *)
  while !accepting_connections do
    let client_socket, client_address = Unix.accept server_socket in

    let client_inet_address =
      match client_address with
      | Unix.ADDR_INET (address, _) -> address
      | _ -> Unix.inet_addr_any
    in
    let client_inet_address_str =
      Unix.string_of_inet_addr client_inet_address
    in
    Log.debug (fun m ->
        m "Recieved client connection, address: %s" client_inet_address_str);

    handle_client client_socket
  done;

  Unix.close server_socket

let run_server port =
  let server_socket = create_server_socket port in
  match server_socket with
  | Some socket ->
      Log.info (fun m -> m "Server listening on port %d" port);
      accept_connection socket
  | None -> ()

let stop_server () = accepting_connections := false
