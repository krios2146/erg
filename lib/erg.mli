type http_method =
  | Get
  | Post
  | Put
  | Delete

type header = Erg_internal.Http_header.t =
  (* General Headers *)
  | CacheControl of string
  | Connection of string
  | Date of string
  | Pragma of string
  | Trailer of string
  | TransferEncoding of string
  | Upgrade of string
  | Via of string
  | Warning of string
  (* Request Headers *)
  | Accept of string
  | AcceptCharset of string
  | AcceptEncoding of string
  | AcceptLanguage of string
  | Authorization of string
  | Expect of string
  | From of string
  | Host of string
  | IfMatch of string
  | IfModifiedSince of string
  | IfNoneMatch of string
  | IfRange of string
  | IfUnmodifiedSince of string
  | MaxForwards of string
  | ProxyAuthorization of string
  | Range of string
  | Referer of string
  | TE of string
  | UserAgent of string
  (* Entity Headers *)
  | Allow of string
  | ContentEncoding of string
  | ContentLanguage of string
  | ContentLength of string
  | ContentLocation of string
  | ContentMD5 of string
  | ContentRange of string
  | ContentType of string
  | Expires of string
  | LastModified of string

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

type http_request
type http_response
type handler
type handlers

(** [start port handlers] Starts the server listening on the specified port with specified handlers *)
val start : int -> handlers -> unit

val empty_handlers : unit -> handlers

(** [add_handler h handlers] Adds the specified handler to the handlers *)
val add_handler : handler -> handlers -> handlers

(* TODO: implement named placeholders *)

(** [create_handler m uri req -> res]

    Creates the [handler] for the specified HTTP method on the [uri]
    with function that transforms [http_request] to [http_response]

    Named placeholders with syntax \{name\} could be used in [uri] to define path parameters *)
val create_handler : http_method -> string -> (http_request -> http_response) -> handler

(** [empty_http_response] Returns "empty" HTTP response *)
val empty_http_response : http_response

(** [set_headers h_list res] Sets specified headers to the response, returns response with headers *)
val set_headers : header list -> http_response -> http_response

(** [set_header h res] Sets specified header to the response, returns response with header *)
val set_header : header -> http_response -> http_response

(** [set_response_body body res] Sets response body to the response, returns response with a body *)
val set_response_body : string -> http_response -> http_response

(** [set_status_code code res] Sets status code for the response, returns response with specified status code *)
val set_status_code : status_code -> http_response -> http_response

(** [get_param req param] Retrieves the optional GET parameter from the request's query *)
val get_param : http_request -> string -> string option

(* TODO: implement *)

(** [req param] Retrieves the optional path param from request *)
(* val path_param : http_request -> string -> string option *)
