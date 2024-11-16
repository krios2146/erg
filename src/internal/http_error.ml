type t =
  | Malformed_request
  | Malformed_request_line
  | Malformed_header
  | Unknown_method
  | Unknown_header

let to_string err =
  match err with
  | Malformed_request -> "Malformed_request"
  | Malformed_request_line -> "Malformed_request_line"
  | Malformed_header -> "Malformed_header"
  | Unknown_method -> "Unknown_method"
  | Unknown_header -> "Unknown_header"
;;
