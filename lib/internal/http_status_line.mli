type t =
  { http_version : string
  ; status_code : int
  ; reason_phrase : string
  }

val to_string : t -> string
