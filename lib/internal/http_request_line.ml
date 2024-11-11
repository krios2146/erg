type t = {
  http_method : Http_method.t;
  request_uri : string;
  http_version : string;
}

let from_string req_line =
  let req_line_parts = String.split_on_char Http_notation.sp req_line in
  let open Http_error in
  match req_line_parts with
  | [] -> Error Malformed_request_line
  | [ _; _ ] -> Error Malformed_request_line
  | [ http_method; uri; version ] -> (
      let http_method = Http_method.from_string http_method in
      match http_method with
      | Ok http_method ->
          Ok { http_method; request_uri = uri; http_version = version }
      | Error e -> Error e)
  | _ -> Error Malformed_request_line

let pretty_print request_line =
  Printf.sprintf "HTTP Method: %s \nRequest URI: %s \nHTTP Version: %s \n"
    (Http_method.to_string request_line.http_method)
    request_line.request_uri request_line.http_version
