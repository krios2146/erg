type t =
  { status_line : Http_status_line.t
  ; headers : Http_header.t list
  ; message_body : string
  }

val to_string : t -> string
val empty : t
val set_headers : Http_header.t list -> t -> t
val set_header : Http_header.t -> t -> t
val set_response_body : string -> t -> t
val set_status_code : Status_code.t -> t -> t
