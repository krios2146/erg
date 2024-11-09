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

val from_string : string -> (t, Http_error.t) result
(** Parses HTTP header from the string *)

val to_string : t -> string
(** Format the HTTP header to the string to be used in HTTP response *)

val pretty_print : t -> string
(** Pretty prints HTTP header. For debug purposes only, do not use in any other way *)
