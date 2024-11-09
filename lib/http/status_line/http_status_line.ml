type t = { http_version : string; status_code : int; reason_phrase : string }

let to_string sl =
  let open Http_notation in
  let sp = String.make 1 sp in
  let status_code = sl.status_code |> string_of_int in
  sl.http_version ^ sp ^ status_code ^ sp ^ sl.reason_phrase ^ crlf
