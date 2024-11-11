type t = {
  status_line : Http_status_line.t;
  headers : Http_header.t list;
  message_body : string;
}

let headers_to_string headers =
  List.map Http_header.to_string headers
  |> List.map (fun h -> h ^ Http_notation.crlf)
  |> String.concat String.empty

let to_string res =
  let status_line = Http_status_line.to_string res.status_line in
  let headers = headers_to_string res.headers in
  let message = String.trim res.message_body in
  status_line ^ headers ^ Http_notation.crlf ^ message ^ Http_notation.crlf
