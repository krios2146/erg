type error =
  | Empty_request_line
  | Incomplete_request_line
  | Malformed_request_line
  | Unknown_method
  | Malformed_request

type http_method =
  | OPTIONS
  | GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | TRACE
  | CONNECT

type request_line = {
  http_method : http_method;
  request_uri : string;
  http_version : string;
}

type http_request = {
  request_line : request_line;
  headers : string list;
  message_body : string;
}

let error_to_string err =
  match err with
  | Empty_request_line -> "Empty_request_line"
  | Incomplete_request_line -> "Incomplete_request_line"
  | Malformed_request_line -> "Malformed_request_line"
  | Unknown_method -> "Unknown_method"
  | Malformed_request -> "Malformed_request"

let http_method_to_string http_method =
  match http_method with
  | OPTIONS -> "OPTIONS"
  | GET -> "GET"
  | HEAD -> "HEAD"
  | POST -> "POST"
  | PUT -> "PUT"
  | DELETE -> "DELETE"
  | TRACE -> "TRACE"
  | CONNECT -> "CONNECT"

let request_line_to_string request_line =
  Printf.sprintf "HTTP Method: %s \nRequest URI: %s \nHTTP Version: %s \n"
    (http_method_to_string request_line.http_method)
    request_line.request_uri request_line.http_version

let headers_to_string headers =
  let rec aux headers acc =
    match headers with hd :: tl -> aux tl (acc ^ hd ^ "\n") | [] -> acc
  in
  aux headers ""

let http_request_to_string http_request =
  Printf.sprintf "Request Line:\n%s\nHeaders:\n%s\nMessage:\n%s\n"
    (request_line_to_string http_request.request_line)
    (headers_to_string http_request.headers)
    http_request.message_body

let parse_http_method http_method =
  match http_method with
  | "OPTIONS" -> Ok OPTIONS
  | "GET" -> Ok GET
  | "HEAD" -> Ok HEAD
  | "POST" -> Ok POST
  | "PUT" -> Ok PUT
  | "DELETE" -> Ok DELETE
  | "TRACE" -> Ok TRACE
  | "CONNECT" -> Ok CONNECT
  | _ -> Error Unknown_method

let http_version = "HTTP/1.1"

(* let cr = '\r' *)
(* let lf = '\n' *)
(* let ht = '\t' *)
let sp = ' '
let crlf = "\r\n"
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

let parse_request_line req_line =
  let req_line_parts = String.split_on_char sp req_line in
  match req_line_parts with
  | [] -> Error Empty_request_line
  | [ _; _ ] -> Error Incomplete_request_line
  | [ http_method; uri; version ] -> (
      let http_method = parse_http_method http_method in
      match http_method with
      | Ok http_method ->
          Ok { http_method; request_uri = uri; http_version = version }
      | Error e -> Error e)
  | _ -> Error Malformed_request_line

let handle_tcp_client_data data =
  Log.debug (fun m -> m "Recieved data:\n%s" data);
  let crlf_delimiter = Str.regexp crlf in
  let request_lines = Str.split crlf_delimiter data in
  match request_lines with
  | hd :: tl -> (
      let request_line = parse_request_line hd in
      match request_line with
      | Ok rl -> Ok { request_line = rl; headers = tl; message_body = "" }
      | Error e -> Error e)
  | _ -> Error Malformed_request

let write_http_response socket data =
  let status_line = http_version ^ " 501 Not Implemented " ^ crlf in
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
     | Ok data -> write_http_response socket (http_request_to_string data));
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

    (* TODO: Run in parallel, so server could serve multiple clients at the same time *)
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
