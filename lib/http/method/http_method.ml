type t = Options | Get | Head | Post | Put | Delete | Trace | Connect

let from_string http_method =
  match http_method with
  | "OPTIONS" -> Ok Options
  | "GET" -> Ok Get
  | "HEAD" -> Ok Head
  | "POST" -> Ok Post
  | "PUT" -> Ok Put
  | "DELETE" -> Ok Delete
  | "TRACE" -> Ok Trace
  | "CONNECT" -> Ok Connect
  | _ -> Error Http_error.Unknown_method

let to_string http_method =
  match http_method with
  | Options -> "OPTIONS"
  | Get -> "GET"
  | Head -> "HEAD"
  | Post -> "POST"
  | Put -> "PUT"
  | Delete -> "DELETE"
  | Trace -> "TRACE"
  | Connect -> "CONNECT"
