let sp = ' '
let crlf = "\r\n"

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

type status_line = {
  http_version : string;
  status_code : int;
  reason_phrase : string;
}

type http_response = {
  status_line : status_line;
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

let pretty_print_header header =
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

let pretty_print_headers (headers : header list) : string =
  String.concat "\n" (List.map pretty_print_header headers)

let pretty_print_request_line request_line =
  Printf.sprintf "HTTP Method: %s \nRequest URI: %s \nHTTP Version: %s \n"
    (http_method_to_string request_line.http_method)
    request_line.request_uri request_line.http_version

let pretty_print_http_request http_request =
  Printf.sprintf "Request Line:\n%s\nHeaders:\n%s\nMessage:\n%s\n"
    (pretty_print_request_line http_request.request_line)
    (pretty_print_headers http_request.headers)
    http_request.message_body

let http_header_to_string header =
  match header with
  | GeneralHeader h -> (
      match h with
      | CacheControl v -> Printf.sprintf "Cache-Control: %s" v
      | Connection v -> Printf.sprintf "Connection: %s" v
      | Date v -> Printf.sprintf "Date: %s" v
      | Pragma v -> Printf.sprintf "Pragma: %s" v
      | Trailer v -> Printf.sprintf "Trailer: %s" v
      | TransferEncoding v -> Printf.sprintf "Transfer-Encoding: %s" v
      | Upgrade v -> Printf.sprintf "Upgrade: %s" v
      | Via v -> Printf.sprintf "Via: %s" v
      | Warning v -> Printf.sprintf "Warning: %s" v)
  | RequestHeader h -> (
      match h with
      | Accept v -> Printf.sprintf "Accept: %s" v
      | AcceptCharset v -> Printf.sprintf "Accept-Charset: %s" v
      | AcceptEncoding v -> Printf.sprintf "Accept-Encoding: %s" v
      | AcceptLanguage v -> Printf.sprintf "Accept-Language: %s" v
      | Authorization v -> Printf.sprintf "Authorization: %s" v
      | Expect v -> Printf.sprintf "Expect: %s" v
      | From v -> Printf.sprintf "From: %s" v
      | Host v -> Printf.sprintf "Host: %s" v
      | IfMatch v -> Printf.sprintf "If-Match: %s" v
      | IfModifiedSince v -> Printf.sprintf "If-Modified-Since: %s" v
      | IfNoneMatch v -> Printf.sprintf "If-None-Match: %s" v
      | IfRange v -> Printf.sprintf "If-Range: %s" v
      | IfUnmodifiedSince v -> Printf.sprintf "If-Unmodified-Since: %s" v
      | MaxForwards v -> Printf.sprintf "Max-Forwards: %s" v
      | ProxyAuthorization v -> Printf.sprintf "Proxy-Authorization: %s" v
      | Range v -> Printf.sprintf "Range: %s" v
      | Referer v -> Printf.sprintf "Referer: %s" v
      | TE v -> Printf.sprintf "TE: %s" v
      | UserAgent v -> Printf.sprintf "User-Agent: %s" v)
  | EntityHeader h -> (
      match h with
      | Allow v -> Printf.sprintf "Allow: %s" v
      | ContentEncoding v -> Printf.sprintf "Content-Encoding: %s" v
      | ContentLanguage v -> Printf.sprintf "Content-Language: %s" v
      | ContentLength v -> Printf.sprintf "Content-Length: %s" v
      | ContentLocation v -> Printf.sprintf "Content-Location: %s" v
      | ContentMD5 v -> Printf.sprintf "Content-MD5: %s" v
      | ContentRange v -> Printf.sprintf "Content-Range: %s" v
      | ContentType v -> Printf.sprintf "Content-Type: %s" v
      | Expires v -> Printf.sprintf "Expires: %s" v
      | LastModified v -> Printf.sprintf "Last-Modified: %s" v)

let http_headers_to_string headers =
  List.map http_header_to_string headers
  |> List.map (fun h -> h ^ crlf)
  |> String.concat String.empty

let status_line_to_string sl =
  let sp = String.make 1 sp in
  let status_code = sl.status_code |> string_of_int in
  sl.http_version ^ sp ^ status_code ^ sp ^ sl.reason_phrase ^ crlf

let http_response_to_string res =
  let status_line = status_line_to_string res.status_line in
  let headers = http_headers_to_string res.headers in
  let message = String.trim res.message_body in
  status_line ^ headers ^ crlf ^ message ^ crlf

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

let write_http_response socket resp =
  let socket_out_channel = Unix.out_channel_of_descr socket in
  let response =
    match resp with
    | Ok resp -> http_response_to_string resp
    | Error e -> http_error_to_string e
  in
  output_string socket_out_channel response;
  flush socket_out_channel

let process_http_request req =
  Result.map
    (fun req ->
      let body = pretty_print_http_request req in
      let headers : header list = [] in
      let headers = GeneralHeader (Connection "keep-alive") :: headers in
      let headers =
        EntityHeader (ContentLength (String.length body |> string_of_int))
        :: headers
      in
      {
        status_line =
          { http_version; status_code = 501; reason_phrase = "Not Implemented" };
        headers;
        message_body = body;
      })
    req

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
    if received_bytes = 0 then if Bytes.length acc > 0 then Some acc else None
    else
      (* Create empty buffer *)
      let updated_data = Bytes.create (Bytes.length acc + received_bytes) in
      (* Copy bytes from previous buffer *)
      Bytes.blit acc 0 updated_data 0 (Bytes.length acc);
      (* Copy received bytes *)
      Bytes.blit buffer 0 updated_data (Bytes.length acc) received_bytes;
      (* Read from tcp again *)
      aux updated_data
  in
  aux Bytes.empty

(* TODO: Don't sure if the Pipelining - 8.1.2.2 is working now *)
(* TODO: TCP socket close on Connection: close header - 8.1.2.1 *)
(* TODO: Use of the 100 (Continue) Status - 8.2.3 *)
(* TODO: Host header handling - 14.23 *)
let rec handle_client socket =
  read_data socket |> Option.map String.of_bytes
  |> Option.map parse_http_request
  |> Option.map process_http_request
  |> Option.iter (write_http_response socket);

  handle_client socket

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

let rec accept_connections server_socket threads =
  (* TODO: Timeouts for connections *)
  if not !accepting_connections then (
    List.iter Thread.join threads;
    Unix.close server_socket)
  else
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
        let thread = Thread.create handle_client client_socket in
        accept_connections server_socket (thread :: threads)
    | Error e ->
        Log.warn (fun m ->
            m "Failed to accept client connection %s" (server_error_to_string e));
        accept_connections server_socket threads

let run_server port =
  let server_socket = create_server_socket port in
  match server_socket with
  | Ok socket ->
      Log.info (fun m -> m "Server listening on port %d" port);
      accept_connections socket []
  | Error Tcp_address_in_use ->
      Log.error (fun m ->
          m "Unable to bind TCP socket, port %d already in use" port)
  | Error _ ->
      Log.error (fun m ->
          m "Unknown error occured while creating server socket on port %d" port)

let stop_server () = accepting_connections := false
