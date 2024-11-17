module Http_method : sig
  type t =
    | Get
    | Post
    | Put
    | Delete
end

module Http_header : sig
  type t =
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
end

module Status_code : sig
  type t =
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
end

module Http_response : sig
  type t

  (** [empty] Returns "empty" HTTP response that must be filled with at least status code *)
  val empty : t

  (** [set_headers res h_list] Sets specified headers to the response, returns response with headers *)
  val set_headers : Http_header.t list -> t -> t

  (** [set_header res h] Sets specified header to the response, returns response with header *)
  val set_header : Http_header.t -> t -> t

  (** [set_response_body res body] Sets response body to the response, returns response with a body *)
  val set_response_body : string -> t -> t

  (** [set_status_code res code] Sets status code for the response, returns response with specified status code *)
  val set_status_code : Status_code.t -> t -> t
end

module Http_request : sig
  type t

  (** [get_param req param] Retrieves the optional GET parameter from the request's query *)
  val get_param : t -> string -> string option

  (** [get_headers req] Retrieves all headers from the request *)
  val get_headers : t -> Http_header.t list

  (** [get_body req] Retrieves untrimmed body of the request as a string *)
  val get_body : t -> string
end

type handler
type handlers

(** [empty_handlers] Returns empty handlers that should be filled with handlers with [add_handler] *)
val empty_handlers : unit -> handlers

(** [add_handler h handlers] Adds the specified handler to the handlers *)
val add_handler : handler -> handlers -> handlers

(** [create_handler m uri req -> res]

    Creates the [handler] for the specified HTTP method on the [uri]
    with function that transforms [http_request] to [http_response] *)
val create_handler
  :  Http_method.t
  -> string
  -> (Http_request.t -> Http_response.t)
  -> handler

(** [start port handlers] Starts the server listening on the specified port with specified handlers *)
val start : int -> handlers -> unit
