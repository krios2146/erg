type t =
  { http_method : Http_method.t
  ; request_uri : string
  ; http_version : string
  }

val from_string : string -> (t, Http_error.t) result
val pretty_print : t -> string
