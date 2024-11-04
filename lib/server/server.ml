type http_error =
  | Malformed_request
  | Malformed_request_line
  | Malformed_header
  | Unknown_method
  | Unknown_header

type server_error = Tcp_address_in_use | Tcp_socket_error | Non_tcp_client

type http_method =
  | Options
  | Get
  | Head
  | Post
  | Put
  | Delete
  | Trace
  | Connect

type general_header =
  | CacheControl of string
  | Connection of string
  | Date of string
  | Pragma of string
  | Trailer of string
  | TransferEncoding of string
  | Upgrade of string
  | Via of string
  | Warning of string

type request_header =
  | Accept of string
  | AcceptCharset of string
  | AcceptEncoding of string
  | AcceptLanguage of string
  | Authorization of string
  | Expect of string
  | From of string
  | Host of string
  | IfMatch of string
  | IfModifiedSince of string
  | IfNoneMatch of string
  | IfRange of string
  | IfUnmodifiedSince of string
  | MaxForwards of string
  | ProxyAuthorization of string
  | Range of string
  | Referer of string
  | TE of string
  | UserAgent of string

type entity_header =
  | Allow of string
  | ContentEncoding of string
  | ContentLanguage of string
  | ContentLength of string
  | ContentLocation of string
  | ContentMD5 of string
  | ContentRange of string
  | ContentType of string
  | Expires of string
  | LastModified of string

type header =
  | GeneralHeader of general_header
  | RequestHeader of request_header
  | EntityHeader of entity_header

type request_line = {
  http_method : http_method;
  request_uri : string;
  http_version : string;
}

type http_request = {
  request_line : request_line;
  headers : header list;
  message_body : string;
}

let http_error_to_string err =
  match err with
  | Malformed_request -> "Malformed_request"
  | Malformed_request_line -> "Malformed_request_line"
  | Malformed_header -> "Malformed_header"
  | Unknown_method -> "Unknown_method"
  | Unknown_header -> "Unknown_header"

let server_error_to_string err =
  match err with
  | Tcp_socket_error -> "Tcp_socket_error"
  | Tcp_address_in_use -> "Tcp_address_in_use"
  | Non_tcp_client -> "Non_tcp_client"

let http_method_to_string http_method =
  match http_method with
  | Options -> "OPTIONS"
  | Get -> "GET"
  | Head -> "HEAD"
  | Post -> "POST"
  | Put -> "PUT"
  | Delete -> "DELETE"
  | Trace -> "TRACE"
  | Connect -> "CONNECT"

let header_to_string header =
  match header with
  | GeneralHeader h -> (
      match h with
      | CacheControl v ->
          Printf.sprintf "General Header: Cache-Control: \"%s\"" v
      | Connection v -> Printf.sprintf "General Header: Connection: \"%s\"" v
      | Date v -> Printf.sprintf "General Header: Date: \"%s\"" v
      | Pragma v -> Printf.sprintf "General Header: Pragma: \"%s\"" v
      | Trailer v -> Printf.sprintf "General Header: Trailer: \"%s\"" v
      | TransferEncoding v ->
          Printf.sprintf "General Header: Transfer-Encoding: \"%s\"" v
      | Upgrade v -> Printf.sprintf "General Header: Upgrade: \"%s\"" v
      | Via v -> Printf.sprintf "General Header: Via: \"%s\"" v
      | Warning v -> Printf.sprintf "General Header: Warning: \"%s\"" v)
  | RequestHeader h -> (
      match h with
      | Accept v -> Printf.sprintf "Request Header: Accept: \"%s\"" v
      | AcceptCharset v ->
          Printf.sprintf "Request Header: Accept-Charset: \"%s\"" v
      | AcceptEncoding v ->
          Printf.sprintf "Request Header: Accept-Encoding: \"%s\"" v
      | AcceptLanguage v ->
          Printf.sprintf "Request Header: Accept-Language: \"%s\"" v
      | Authorization v ->
          Printf.sprintf "Request Header: Authorization: \"%s\"" v
      | Expect v -> Printf.sprintf "Request Header: Expect: \"%s\"" v
      | From v -> Printf.sprintf "Request Header: From: \"%s\"" v
      | Host v -> Printf.sprintf "Request Header: Host: \"%s\"" v
      | IfMatch v -> Printf.sprintf "Request Header: If-Match: \"%s\"" v
      | IfModifiedSince v ->
          Printf.sprintf "Request Header: If-Modified-Since: \"%s\"" v
      | IfNoneMatch v ->
          Printf.sprintf "Request Header: If-None-Match: \"%s\"" v
      | IfRange v -> Printf.sprintf "Request Header: If-Range: \"%s\"" v
      | IfUnmodifiedSince v ->
          Printf.sprintf "Request Header: If-Unmodified-Since: \"%s\"" v
      | MaxForwards v -> Printf.sprintf "Request Header: Max-Forwards: \"%s\"" v
      | ProxyAuthorization v ->
          Printf.sprintf "Request Header: Proxy-Authorization: \"%s\"" v
      | Range v -> Printf.sprintf "Request Header: Range: \"%s\"" v
      | Referer v -> Printf.sprintf "Request Header: Referer: \"%s\"" v
      | TE v -> Printf.sprintf "Request Header: TE: \"%s\"" v
      | UserAgent v -> Printf.sprintf "Request Header: User-Agent: \"%s\"" v)
  | EntityHeader h -> (
      match h with
      | Allow v -> Printf.sprintf "Entity Header: Allow: \"%s\"" v
      | ContentEncoding v ->
          Printf.sprintf "Entity Header: Content-Encoding: \"%s\"" v
      | ContentLanguage v ->
          Printf.sprintf "Entity Header: Content-Language: \"%s\"" v
      | ContentLength v ->
          Printf.sprintf "Entity Header: Content-Length: \"%s\"" v
      | ContentLocation v ->
          Printf.sprintf "Entity Header: Content-Location: \"%s\"" v
      | ContentMD5 v -> Printf.sprintf "Entity Header: Content-MD5: \"%s\"" v
      | ContentRange v ->
          Printf.sprintf "Entity Header: Content-Range: \"%s\"" v
      | ContentType v -> Printf.sprintf "Entity Header: Content-Type: \"%s\"" v
      | Expires v -> Printf.sprintf "Entity Header: Expires: \"%s\"" v
      | LastModified v ->
          Printf.sprintf "Entity Header: Last-Modified: \"%s\"" v)

let headers_to_string (headers : header list) : string =
  String.concat "\n" (List.map header_to_string headers)

let request_line_to_string request_line =
  Printf.sprintf "HTTP Method: %s \nRequest URI: %s \nHTTP Version: %s \n"
    (http_method_to_string request_line.http_method)
    request_line.request_uri request_line.http_version

let http_request_to_string http_request =
  Printf.sprintf "Request Line:\n%s\nHeaders:\n%s\nMessage:\n%s\n"
    (request_line_to_string http_request.request_line)
    (headers_to_string http_request.headers)
    http_request.message_body

let parse_http_method http_method =
  match http_method with
  | "OPTIONS" -> Ok Options
  | "GET" -> Ok Get
  | "HEAD" -> Ok Head
  | "POST" -> Ok Post
  | "PUT" -> Ok Put
  | "DELETE" -> Ok Delete
  | "TRACE" -> Ok Trace
  | "CONNECT" -> Ok Connect
  | _ -> Error Unknown_method

let parse_header header =
  let header_parts = String.split_on_char ':' header in
  match header_parts with
  | [ header_name; header_value ] -> (
      let header_value = String.trim header_value in
      match String.lowercase_ascii header_name with
      | "cache-control" -> Ok (GeneralHeader (CacheControl header_value))
      | "connection" -> Ok (GeneralHeader (Connection header_value))
      | "date" -> Ok (GeneralHeader (Date header_value))
      | "pragma" -> Ok (GeneralHeader (Pragma header_value))
      | "trailer" -> Ok (GeneralHeader (Trailer header_value))
      | "transfer-encoding" ->
          Ok (GeneralHeader (TransferEncoding header_value))
      | "upgrade" -> Ok (GeneralHeader (Upgrade header_value))
      | "via" -> Ok (GeneralHeader (Via header_value))
      | "warning" -> Ok (GeneralHeader (Warning header_value))
      | "accept" -> Ok (RequestHeader (Accept header_value))
      | "accept-charset" -> Ok (RequestHeader (AcceptCharset header_value))
      | "accept-encoding" -> Ok (RequestHeader (AcceptEncoding header_value))
      | "accept-language" -> Ok (RequestHeader (AcceptLanguage header_value))
      | "authorization" -> Ok (RequestHeader (Authorization header_value))
      | "expect" -> Ok (RequestHeader (Expect header_value))
      | "from" -> Ok (RequestHeader (From header_value))
      | "host" -> Ok (RequestHeader (Host header_value))
      | "if-match" -> Ok (RequestHeader (IfMatch header_value))
      | "if-modifiedsince" -> Ok (RequestHeader (IfModifiedSince header_value))
      | "if-nonematch" -> Ok (RequestHeader (IfNoneMatch header_value))
      | "if-range" -> Ok (RequestHeader (IfRange header_value))
      | "if-unmodifiedsince" ->
          Ok (RequestHeader (IfUnmodifiedSince header_value))
      | "max-forwards" -> Ok (RequestHeader (MaxForwards header_value))
      | "proxy-authorization" ->
          Ok (RequestHeader (ProxyAuthorization header_value))
      | "range" -> Ok (RequestHeader (Range header_value))
      | "referer" -> Ok (RequestHeader (Referer header_value))
      | "te" -> Ok (RequestHeader (TE header_value))
      | "user-agent" -> Ok (RequestHeader (UserAgent header_value))
      | "allow" -> Ok (EntityHeader (Allow header_value))
      | "content-encoding" -> Ok (EntityHeader (ContentEncoding header_value))
      | "content-language" -> Ok (EntityHeader (ContentLanguage header_value))
      | "content-length" -> Ok (EntityHeader (ContentLength header_value))
      | "content-location" -> Ok (EntityHeader (ContentLocation header_value))
      | "content-md5" -> Ok (EntityHeader (ContentMD5 header_value))
      | "content-range" -> Ok (EntityHeader (ContentRange header_value))
      | "content-type" -> Ok (EntityHeader (ContentType header_value))
      | "expires" -> Ok (EntityHeader (Expires header_value))
      | "last-modified" -> Ok (EntityHeader (LastModified header_value))
      | _ -> Error Unknown_header)
  | _ -> Error Malformed_header

let parse_headers headers =
  List.map parse_header headers
  |> List.filter Result.is_ok |> List.map Result.get_ok

let http_version = "HTTP/1.1"

(* WARN: unused still *)
(* let cr = '\r' *)
(* let lf = '\n' *)
(* let ht = '\t' *)

let sp = ' '
let crlf = "\r\n"

let parse_request_line req_line =
  let req_line_parts = String.split_on_char sp req_line in
  match req_line_parts with
  | [] -> Error Malformed_request_line
  | [ _; _ ] -> Error Malformed_request_line
  | [ http_method; uri; version ] -> (
      let http_method = parse_http_method http_method in
      match http_method with
      | Ok http_method ->
          Ok { http_method; request_uri = uri; http_version = version }
      | Error e -> Error e)
  | _ -> Error Malformed_request_line

let first_index_of sub s =
  let sub_len = String.length sub in
  let s_len = String.length s in
  let rec aux i =
    if s_len < i + sub_len then None
    else if String.sub s i sub_len = sub then Some i
    else aux (i + 1)
  in
  aux 0

let split_on_first sub s =
  let sub_len = String.length sub in
  let s_len = String.length s in
  if s_len < sub_len then None
  else
    let sub_index = first_index_of sub s in
    match sub_index with
    | None -> None
    | Some i ->
        let left = String.sub s 0 i in
        let right_start_index = i + sub_len in
        let right_len = s_len - right_start_index in
        let right = String.sub s right_start_index right_len in
        Some (left, right)

let parse_http_request data =
  let rec aux data acc =
    match split_on_first crlf data with
    | None -> Error Malformed_request
    | Some (l, r) ->
        if String.length l = 0 then Ok (List.rev acc, r) else aux r (l :: acc)
  in
  match split_on_first crlf data with
  | None -> Error Malformed_request
  | Some (l, r) -> (
      let request_line = parse_request_line l in
      let headers_and_body = aux r [] in
      match (request_line, headers_and_body) with
      | Error e, _ -> Error e
      | _, Error e -> Error e
      | Ok request_line, Ok (headers, body) ->
          let headers = parse_headers headers in
          Ok { request_line; headers; message_body = body })

let write_http_response socket data =
  let status_line = http_version ^ " 501 Not Implemented " ^ crlf in
  let entity = data in
  let response = status_line ^ crlf ^ entity in
  let response_bytes = Bytes.of_string response in
  let response_length = Bytes.length response_bytes in
  ignore (Unix.write socket response_bytes 0 response_length)

let handle_tcp_client_data data =
  Log.debug (fun m -> m "Recieved data:\n%s" data);
  parse_http_request data

let handle_client socket =
  let buffer = Bytes.create 1024 in
  let received_bytes = Unix.read socket buffer 0 1024 in

  (if received_bytes > 0 then
     let data = Bytes.sub_string buffer 0 received_bytes in
     let handeled_data = handle_tcp_client_data data in
     match handeled_data with
     | Error e -> write_http_response socket (http_error_to_string e)
     | Ok data -> write_http_response socket (http_request_to_string data));
  Unix.close socket

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

(* TODO: Run in parallel, so server could serve multiple clients at the same time *)
(* TODO: Persistent TCP connections *)
let accept_connection server_socket =
  Log.debug (fun m -> m "Accepting connections");

  (* Not sure whether doing it recursively is a good idea, so there is a loop *)
  while !accepting_connections do
    let client_socket, client_address = Unix.accept server_socket in

    let client_inet_address =
      match client_address with
      | Unix.ADDR_INET (address, _) -> Ok address
      | _ -> Error Non_tcp_client
    in
    let client_inet_address =
      Result.map Unix.string_of_inet_addr client_inet_address
    in
    if Result.is_ok client_inet_address then (
      Log.debug (fun m ->
          m "Recieved client connection, address: %s"
            (Result.get_ok client_inet_address));

      handle_client client_socket)
    else
      Log.error (fun m ->
          m "Failed to accept client connection %s"
            (server_error_to_string (Result.get_error client_inet_address)))
  done;

  Log.info (fun m -> m "Accepting connections");
  Unix.close server_socket

let run_server port =
  let server_socket = create_server_socket port in
  match server_socket with
  | Ok socket ->
      Log.info (fun m -> m "Server listening on port %d" port);
      accept_connection socket
  | Error Tcp_address_in_use ->
      Log.error (fun m ->
          m "Unable to bind TCP socket, port %d already in use" port)
  | Error _ ->
      Log.error (fun m ->
          m "Unknown error occured while creating server socket on port %d" port)

let stop_server () = accepting_connections := false
