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

let from_string header =
  let header_parts = String.split_on_char ':' header in
  match header_parts with
  | [ header_name; header_value ] ->
    let value = String.trim header_value in
    (match String.lowercase_ascii header_name with
     | "cache-control" -> Ok (CacheControl value)
     | "connection" -> Ok (Connection value)
     | "date" -> Ok (Date value)
     | "pragma" -> Ok (Pragma value)
     | "trailer" -> Ok (Trailer value)
     | "transfer-encoding" -> Ok (TransferEncoding value)
     | "upgrade" -> Ok (Upgrade value)
     | "via" -> Ok (Via value)
     | "warning" -> Ok (Warning value)
     | "accept" -> Ok (Accept value)
     | "accept-charset" -> Ok (AcceptCharset value)
     | "accept-encoding" -> Ok (AcceptEncoding value)
     | "accept-language" -> Ok (AcceptLanguage value)
     | "authorization" -> Ok (Authorization value)
     | "expect" -> Ok (Expect value)
     | "from" -> Ok (From value)
     | "host" -> Ok (Host value)
     | "if-match" -> Ok (IfMatch value)
     | "if-modifiedsince" -> Ok (IfModifiedSince value)
     | "if-nonematch" -> Ok (IfNoneMatch value)
     | "if-range" -> Ok (IfRange value)
     | "if-unmodifiedsince" -> Ok (IfUnmodifiedSince value)
     | "max-forwards" -> Ok (MaxForwards value)
     | "proxy-authorization" -> Ok (ProxyAuthorization value)
     | "range" -> Ok (Range value)
     | "referer" -> Ok (Referer value)
     | "te" -> Ok (TE value)
     | "user-agent" -> Ok (UserAgent value)
     | "allow" -> Ok (Allow value)
     | "content-encoding" -> Ok (ContentEncoding value)
     | "content-language" -> Ok (ContentLanguage value)
     | "content-length" -> Ok (ContentLength value)
     | "content-location" -> Ok (ContentLocation value)
     | "content-md5" -> Ok (ContentMD5 value)
     | "content-range" -> Ok (ContentRange value)
     | "content-type" -> Ok (ContentType value)
     | "expires" -> Ok (Expires value)
     | "last-modified" -> Ok (LastModified value)
     | _ -> Error Http_error.Unknown_header)
  | _ -> Error Http_error.Malformed_header
;;

let to_string header =
  match header with
  | CacheControl v -> Printf.sprintf "Cache-Control: %s" v
  | Connection v -> Printf.sprintf "Connection: %s" v
  | Date v -> Printf.sprintf "Date: %s" v
  | Pragma v -> Printf.sprintf "Pragma: %s" v
  | Trailer v -> Printf.sprintf "Trailer: %s" v
  | TransferEncoding v -> Printf.sprintf "Transfer-Encoding: %s" v
  | Upgrade v -> Printf.sprintf "Upgrade: %s" v
  | Via v -> Printf.sprintf "Via: %s" v
  | Warning v -> Printf.sprintf "Warning: %s" v
  | Accept v -> Printf.sprintf "Accept: %s" v
  | AcceptCharset v -> Printf.sprintf "Accept-Charset: %s" v
  | AcceptEncoding v -> Printf.sprintf "Accept-Encoding: %s" v
  | AcceptLanguage v -> Printf.sprintf "Accept-Language: %s" v
  | Authorization v -> Printf.sprintf "Authorization: %s" v
  | Expect v -> Printf.sprintf "Expect: %s" v
  | From v -> Printf.sprintf "From: %s" v
  | Host v -> Printf.sprintf "Host: %s" v
  | IfMatch v -> Printf.sprintf "If-Match: %s" v
  | IfModifiedSince v -> Printf.sprintf "If-Modified-Since: %s" v
  | IfNoneMatch v -> Printf.sprintf "If-None-Match: %s" v
  | IfRange v -> Printf.sprintf "If-Range: %s" v
  | IfUnmodifiedSince v -> Printf.sprintf "If-Unmodified-Since: %s" v
  | MaxForwards v -> Printf.sprintf "Max-Forwards: %s" v
  | ProxyAuthorization v -> Printf.sprintf "Proxy-Authorization: %s" v
  | Range v -> Printf.sprintf "Range: %s" v
  | Referer v -> Printf.sprintf "Referer: %s" v
  | TE v -> Printf.sprintf "TE: %s" v
  | UserAgent v -> Printf.sprintf "User-Agent: %s" v
  | Allow v -> Printf.sprintf "Allow: %s" v
  | ContentEncoding v -> Printf.sprintf "Content-Encoding: %s" v
  | ContentLanguage v -> Printf.sprintf "Content-Language: %s" v
  | ContentLength v -> Printf.sprintf "Content-Length: %s" v
  | ContentLocation v -> Printf.sprintf "Content-Location: %s" v
  | ContentMD5 v -> Printf.sprintf "Content-MD5: %s" v
  | ContentRange v -> Printf.sprintf "Content-Range: %s" v
  | ContentType v -> Printf.sprintf "Content-Type: %s" v
  | Expires v -> Printf.sprintf "Expires: %s" v
  | LastModified v -> Printf.sprintf "Last-Modified: %s" v
;;

let pretty_print header =
  match header with
  | CacheControl v -> Printf.sprintf "General Header: Cache-Control: \"%s\"" v
  | Connection v -> Printf.sprintf "General Header: Connection: \"%s\"" v
  | Date v -> Printf.sprintf "General Header: Date: \"%s\"" v
  | Pragma v -> Printf.sprintf "General Header: Pragma: \"%s\"" v
  | Trailer v -> Printf.sprintf "General Header: Trailer: \"%s\"" v
  | TransferEncoding v -> Printf.sprintf "General Header: Transfer-Encoding: \"%s\"" v
  | Upgrade v -> Printf.sprintf "General Header: Upgrade: \"%s\"" v
  | Via v -> Printf.sprintf "General Header: Via: \"%s\"" v
  | Warning v -> Printf.sprintf "General Header: Warning: \"%s\"" v
  | Accept v -> Printf.sprintf "Request Header: Accept: \"%s\"" v
  | AcceptCharset v -> Printf.sprintf "Request Header: Accept-Charset: \"%s\"" v
  | AcceptEncoding v -> Printf.sprintf "Request Header: Accept-Encoding: \"%s\"" v
  | AcceptLanguage v -> Printf.sprintf "Request Header: Accept-Language: \"%s\"" v
  | Authorization v -> Printf.sprintf "Request Header: Authorization: \"%s\"" v
  | Expect v -> Printf.sprintf "Request Header: Expect: \"%s\"" v
  | From v -> Printf.sprintf "Request Header: From: \"%s\"" v
  | Host v -> Printf.sprintf "Request Header: Host: \"%s\"" v
  | IfMatch v -> Printf.sprintf "Request Header: If-Match: \"%s\"" v
  | IfModifiedSince v -> Printf.sprintf "Request Header: If-Modified-Since: \"%s\"" v
  | IfNoneMatch v -> Printf.sprintf "Request Header: If-None-Match: \"%s\"" v
  | IfRange v -> Printf.sprintf "Request Header: If-Range: \"%s\"" v
  | IfUnmodifiedSince v -> Printf.sprintf "Request Header: If-Unmodified-Since: \"%s\"" v
  | MaxForwards v -> Printf.sprintf "Request Header: Max-Forwards: \"%s\"" v
  | ProxyAuthorization v -> Printf.sprintf "Request Header: Proxy-Authorization: \"%s\"" v
  | Range v -> Printf.sprintf "Request Header: Range: \"%s\"" v
  | Referer v -> Printf.sprintf "Request Header: Referer: \"%s\"" v
  | TE v -> Printf.sprintf "Request Header: TE: \"%s\"" v
  | UserAgent v -> Printf.sprintf "Request Header: User-Agent: \"%s\"" v
  | Allow v -> Printf.sprintf "Entity Header: Allow: \"%s\"" v
  | ContentEncoding v -> Printf.sprintf "Entity Header: Content-Encoding: \"%s\"" v
  | ContentLanguage v -> Printf.sprintf "Entity Header: Content-Language: \"%s\"" v
  | ContentLength v -> Printf.sprintf "Entity Header: Content-Length: \"%s\"" v
  | ContentLocation v -> Printf.sprintf "Entity Header: Content-Location: \"%s\"" v
  | ContentMD5 v -> Printf.sprintf "Entity Header: Content-MD5: \"%s\"" v
  | ContentRange v -> Printf.sprintf "Entity Header: Content-Range: \"%s\"" v
  | ContentType v -> Printf.sprintf "Entity Header: Content-Type: \"%s\"" v
  | Expires v -> Printf.sprintf "Entity Header: Expires: \"%s\"" v
  | LastModified v -> Printf.sprintf "Entity Header: Last-Modified: \"%s\"" v
;;
