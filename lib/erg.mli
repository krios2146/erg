type http_method =
  | Get
  | Post
  | Put
  | Delete

type http_request
type http_response
type handler
type handlers

(** [p] Starts the server listening on the specified port *)
val start : int -> handlers -> unit

(** [h handlers] Adds the specified handler to the handlers list *)
val add_handler : handler -> handlers -> handlers

(** [m uri req -> res]

    Creates the [handler] for the specified HTTP method on the [uri]
    with function that transforms [http_request] to [http_response]

    Named placeholders with syntax \{name\} could be used in [uri] to define path parameters *)
val create_handler : http_method -> string -> (http_request -> http_response) -> handler

(* TODO: implement *)

(** [req q] Retrieves the optional query from request *)
(* val query : http_request -> string -> string option *)

(* TODO: implement *)

(** [req param] Retrieves the optional path param from request *)
(* val path_param : http_request -> string -> string option *)
