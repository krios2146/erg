type t = Options | Get | Head | Post | Put | Delete | Trace | Connect

val from_string : string -> (t, Http_error.t) result
(** Parses HTTP method from string *)

val to_string : t -> string
(** Converts HTTP method to string for use in HTTP response *)
