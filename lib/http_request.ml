type t = {
  request_line : Http_request_line.t;
  headers : Http_header.t list;
  message_body : string;
}

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

let parse_headers headers =
  List.map Http_header.from_string headers
  |> List.filter Result.is_ok |> List.map Result.get_ok

let from_string data =
  let open Http_error in
  let open Http_notation in
  let rec aux data acc =
    match split_on_first crlf data with
    | None -> Error Malformed_request
    | Some (l, r) ->
        if String.length l = 0 then Ok (List.rev acc, r) else aux r (l :: acc)
  in
  match split_on_first crlf data with
  | None -> Error Malformed_request
  | Some (l, r) -> (
      let request_line = Http_request_line.from_string l in
      let headers_and_body = aux r [] in
      match (request_line, headers_and_body) with
      | Error e, _ -> Error e
      | _, Error e -> Error e
      | Ok request_line, Ok (headers, body) ->
          let headers = parse_headers headers in
          Ok { request_line; headers; message_body = body })

let pretty_print_headers (headers : Http_header.t list) : string =
  String.concat "\n" (List.map Http_header.pretty_print headers)

let pretty_print http_request =
  Printf.sprintf "Request Line:\n%s\nHeaders:\n%s\nMessage:\n%s\n"
    (Http_request_line.pretty_print http_request.request_line)
    (pretty_print_headers http_request.headers)
    http_request.message_body
