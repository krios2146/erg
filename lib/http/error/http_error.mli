type t =
  | Malformed_request
  | Malformed_request_line
  | Malformed_header
  | Unknown_method
  | Unknown_header

val to_string : t -> string
