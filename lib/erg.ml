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
let query = Http_request.query
(* let get_param = Http_request.get_param *)
