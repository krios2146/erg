type t = {
  request_line : Http_request_line.t;
  headers : Http_header.t list;
  message_body : string;
}

val from_string : string -> (t, Http_error.t) result
val pretty_print : t -> string
