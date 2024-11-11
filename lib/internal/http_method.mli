type t =
  | Options
  | Get
  | Head
  | Post
  | Put
  | Delete
  | Trace
  | Connect

(** Parses HTTP method from string *)
val from_string : string -> (t, Http_error.t) result

(** Converts HTTP method to string for use in HTTP response *)
val to_string : t -> string
