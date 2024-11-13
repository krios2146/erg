open Erg_internal

type http_method =
  | Get
  | Post
  | Put
  | Delete

type http_request = Http_request.t
type http_response = Http_response.t
type handler = Handler.t
type handlers = Handlers.t

let start port handlers = Server.run port handlers

let create_handler http_method uri func =
  let http_method =
    match http_method with
    | Get -> Http_method.Get
    | Post -> Http_method.Post
    | Put -> Http_method.Put
    | Delete -> Http_method.Delete
  in
  Handler.create_handler http_method uri func
;;

let add_handler h handlers = Handlers.add_handler h handlers
