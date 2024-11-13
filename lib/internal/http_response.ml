type t =
  { status_line : Http_status_line.t
  ; headers : Http_header.t list
  ; message_body : string
  }

let headers_to_string headers =
  List.map Http_header.to_string headers
  |> List.map (fun h -> h ^ Http_notation.crlf)
  |> String.concat String.empty
;;

let to_string res =
  let status_line = Http_status_line.to_string res.status_line in
  let headers = headers_to_string res.headers in
  let message = String.trim res.message_body in
  status_line ^ headers ^ Http_notation.crlf ^ message ^ Http_notation.crlf
;;

let empty =
  { status_line =
      { http_version = Http_notation.http_version; status_code = 0; reason_phrase = "" }
  ; headers = []
  ; message_body = ""
  }
;;

let set_headers headers res =
  let prev_headers = res.headers in
  let new_headers = prev_headers @ headers in
  { res with headers = new_headers }
;;

let set_header header res = { res with headers = header :: res.headers }
let set_response_body body res = { res with message_body = body }

let set_status_code status_code res =
  let open Status_code in
  let status_code, phrase =
    match status_code with
    | Continue -> 100, "Continue"
    | Switching_Protocols -> 101, "Switching Protocols"
    | OK -> 200, "OK"
    | Created -> 201, "Created"
    | Accepted -> 202, "Accepted"
    | Non_Authoritative_Information -> 203, "Non Authoritative Information"
    | No_Content -> 204, "No Content"
    | Reset_Content -> 205, "Reset Content"
    | Partial_Content -> 206, "Partial Content"
    | Multiple_Choices -> 300, "Multiple Choices"
    | Moved_Permanently -> 301, "Moved Permanently"
    | Found -> 302, "Found"
    | See_Other -> 303, "See Other"
    | Not_Modified -> 304, "Not Modified"
    | Use_Proxy -> 305, "Use Proxy"
    | Temporary_Redirect -> 307, "Temporary Redirect"
    | Bad_Request -> 400, "Bad Request"
    | Unauthorized -> 401, "Unauthorized"
    | Payment_Required -> 402, "Payment Required"
    | Forbidden -> 403, "Forbidden"
    | Not_Found -> 404, "Not Found"
    | Method_Not_Allowed -> 405, "Method Not Allowed"
    | Not_Acceptable -> 406, "Not Acceptable"
    | Proxy_Authentication_Required -> 407, "Proxy Authentication Required"
    | Request_Timeout -> 408, "Request Timeout"
    | Conflict -> 409, "Conflict"
    | Gone -> 410, "Gone"
    | Length_Required -> 411, "Length Required"
    | Precondition_Failed -> 412, "Precondition Failed"
    | Request_Entity_Too_Large -> 413, "Request Entity Too Large"
    | Request_URI_Too_Long -> 414, "Request URI Too Long"
    | Unsupported_Media_Type -> 415, "Unsupported Media Type"
    | Requested_Range_Not_Satisfiable -> 416, "Requested Range Not Satisfiable"
    | Expectation_Failed -> 417, "Expectation Failed"
    | Internal_Server_Error -> 500, "Internal Server Error"
    | Not_Implemented -> 501, "Not Implemented"
    | Bad_Gateway -> 502, "Bad Gateway"
    | Service_Unavailable -> 503, "Service Unavailable"
    | Gateway_Timeout -> 504, "Gateway Timeout"
    | HTTP_Version_Not_Supported -> 505, "HTTP Version Not Supported"
  in
  { res with status_line = { res.status_line with status_code; reason_phrase = phrase } }
;;
