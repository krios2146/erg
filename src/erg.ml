module Http_method = struct
  type t =
    | Get
    | Post
    | Put
    | Delete
end

module Http_header = struct
  type t = Erg_internal.Http_header.t =
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

module Status_code = struct
  type t = Erg_internal.Status_code.t =
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

module Http_response = struct
  open Erg_internal

  type t = Http_response.t

  let empty = Http_response.empty
  let set_headers = Http_response.set_headers
  let set_header = Http_response.set_header
  let set_response_body = Http_response.set_response_body
  let set_status_code = Http_response.set_status_code
end

module Http_request = struct
  open Erg_internal

  type t = Http_request.t

  let get_param = Http_request.get_param
  let get_headers = Http_request.get_headers
  let get_body = Http_request.get_body
end

type handler = Erg_internal.Handler.t
type handlers = Erg_internal.Handlers.t

let start port handlers = Erg_internal.Server.run port handlers
let empty_handlers = Erg_internal.Handlers.empty

let create_handler http_method uri func =
  let http_method =
    match http_method with
    | Http_method.Get -> Erg_internal.Http_method.Get
    | Http_method.Post -> Erg_internal.Http_method.Post
    | Http_method.Put -> Erg_internal.Http_method.Put
    | Http_method.Delete -> Erg_internal.Http_method.Delete
  in
  Erg_internal.Handler.create_handler http_method uri func
;;

let add_handler h handlers = Erg_internal.Handlers.add_handler h handlers
