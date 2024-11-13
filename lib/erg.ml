open Erg_internal

type http_method =
  | Get
  | Post
  | Put
  | Delete

type status_code = Erg_internal.Status_code.t =
  | Continue (** 100 *)
  | Switching_Protocols (** 101 *)
  | OK (** 200 *)
  | Created (** 201 *)
  | Accepted (** 202 *)
  | Non_Authoritative_Information (** 203 *)
  | No_Content (** 204 *)
  | Reset_Content (** 205 *)
  | Partial_Content (** 206 *)
  | Multiple_Choices (** 300 *)
  | Moved_Permanently (** 301 *)
  | Found (** 302 *)
  | See_Other (** 303 *)
  | Not_Modified (** 304 *)
  | Use_Proxy (** 305 *)
  | Temporary_Redirect (** 307 *)
  | Bad_Request (** 400 *)
  | Unauthorized (** 401 *)
  | Payment_Required (** 402 *)
  | Forbidden (** 403 *)
  | Not_Found (** 404 *)
  | Method_Not_Allowed (** 405 *)
  | Not_Acceptable (** 406 *)
  | Proxy_Authentication_Required (** 407 *)
  | Request_Timeout (** 408 *)
  | Conflict (** 409 *)
  | Gone (** 410 *)
  | Length_Required (** 411 *)
  | Precondition_Failed (** 412 *)
  | Request_Entity_Too_Large (** 413 *)
  | Request_URI_Too_Long (** 414 *)
  | Unsupported_Media_Type (** 415 *)
  | Requested_Range_Not_Satisfiable (** 416 *)
  | Expectation_Failed (** 417 *)
  | Internal_Server_Error (** 500 *)
  | Not_Implemented (** 501 *)
  | Bad_Gateway (** 502 *)
  | Service_Unavailable (** 503 *)
  | Gateway_Timeout (** 504 *)
  | HTTP_Version_Not_Supported (** 505 *)

type http_request = Http_request.t
type http_response = Http_response.t
type handler = Handler.t
type handlers = Handlers.t
type header = Http_header.t

let start port handlers = Server.run port handlers
let empty_handlers = Handlers.empty

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
let empty_http_response = Http_response.empty
let set_headers = Http_response.set_headers
let set_header = Http_response.set_header
let set_response_body = Http_response.set_response_body
let set_status_code = Http_response.set_status_code
