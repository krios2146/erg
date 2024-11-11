type t = {
  status_line : Http_status_line.t;
  headers : Http_header.t list;
  message_body : string;
}

val to_string : t -> string
