type t =
  { http_method : Http_method.t
  ; request_uri : string
  ; http_version : string
  ; query : Query.t
  }

val from_string : string -> (t, Http_error.t) result
val pretty_print : t -> string
