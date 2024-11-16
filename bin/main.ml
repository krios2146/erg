(* TODO: Logs setup should be deleted before release *)
Fmt_tty.setup_std_outputs ();
Logs.set_level (Some Logs.Debug);
Logs.set_reporter (Logs_fmt.reporter ());
let pretty_print header =
  let open Erg.Http_header in
  match header with
  | CacheControl v -> Printf.sprintf "\"Cache-Control\": \"%s\"" v
  | Connection v -> Printf.sprintf "\"Connection\": \"%s\"" v
  | Date v -> Printf.sprintf "\"Date\": \"%s\"" v
  | Pragma v -> Printf.sprintf "\"Pragma\": \"%s\"" v
  | Trailer v -> Printf.sprintf "\"Trailer\": \"%s\"" v
  | TransferEncoding v -> Printf.sprintf "\"Transfer-Encoding\": \"%s\"" v
  | Upgrade v -> Printf.sprintf "\"Upgrade\": \"%s\"" v
  | Via v -> Printf.sprintf "\"Via\": \"%s\"" v
  | Warning v -> Printf.sprintf "\"Warning\": \"%s\"" v
  | Accept v -> Printf.sprintf "\"Accept\": \"%s\"" v
  | AcceptCharset v -> Printf.sprintf "\"Accept-Charset\": \"%s\"" v
  | AcceptEncoding v -> Printf.sprintf "\"Accept-Encoding\": \"%s\"" v
  | AcceptLanguage v -> Printf.sprintf "\"Accept-Language\": \"%s\"" v
  | Authorization v -> Printf.sprintf "\"Authorization\": \"%s\"" v
  | Expect v -> Printf.sprintf "\"Expect\": \"%s\"" v
  | From v -> Printf.sprintf "\"From\": \"%s\"" v
  | Host v -> Printf.sprintf "\"Host\": \"%s\"" v
  | IfMatch v -> Printf.sprintf "\"If-Match\": \"%s\"" v
  | IfModifiedSince v -> Printf.sprintf "\"If-Modified-Since\": \"%s\"" v
  | IfNoneMatch v -> Printf.sprintf "\"If-None-Match\": \"%s\"" v
  | IfRange v -> Printf.sprintf "\"If-Range\": \"%s\"" v
  | IfUnmodifiedSince v -> Printf.sprintf "\"If-Unmodified-Since\": \"%s\"" v
  | MaxForwards v -> Printf.sprintf "\"Max-Forwards\": \"%s\"" v
  | ProxyAuthorization v -> Printf.sprintf "\"Proxy-Authorization\": \"%s\"" v
  | Range v -> Printf.sprintf "\"Range\": \"%s\"" v
  | Referer v -> Printf.sprintf "\"Referer\": \"%s\"" v
  | TE v -> Printf.sprintf "\"TE\": \"%s\"" v
  | UserAgent v -> Printf.sprintf "\"User-Agent\": \"%s\"" v
  | Allow v -> Printf.sprintf "\"Allow\": \"%s\"" v
  | ContentEncoding v -> Printf.sprintf "\"Content-Encoding\": \"%s\"" v
  | ContentLanguage v -> Printf.sprintf "\"Content-Language\": \"%s\"" v
  | ContentLength v -> Printf.sprintf "\"Content-Length\": \"%s\"" v
  | ContentLocation v -> Printf.sprintf "\"Content-Location\": \"%s\"" v
  | ContentMD5 v -> Printf.sprintf "\"Content-MD5\": \"%s\"" v
  | ContentRange v -> Printf.sprintf "\"Content-Range\": \"%s\"" v
  | ContentType v -> Printf.sprintf "\"Content-Type\": \"%s\"" v
  | Expires v -> Printf.sprintf "\"Expires\": \"%s\"" v
  | LastModified v -> Printf.sprintf "\"Last-Modified\": \"%s\"" v
in
let handlers = Erg.empty_handlers () in
let handler =
  Erg.create_handler Erg.Http_method.Get "/currencies" (fun req ->
    let query =
      match Erg.Http_request.get_param req "some" with
      | None -> "No some parameter"
      | Some q -> "some parameter is: " ^ q
    in
    let headers =
      Erg.Http_request.get_headers req
      |> List.map pretty_print
      |> List.map (fun h -> h ^ ";\n\t\t")
      |> List.fold_left String.cat String.empty
    in
    let req_body = Erg.Http_request.get_body req in
    let response =
      Printf.sprintf
        "{\n\
         \t\"message\": \"It's working\";\n\
         \t\"query\": \"%s\";\n\
         \t\"headers\": [%s];\n\
         \t\"body\": \"%s\";\n\
         }"
        query
        headers
        req_body
    in
    Erg.Http_response.empty
    |> Erg.Http_response.set_status_code Erg.Status_code.OK
    |> Erg.Http_response.set_header (Erg.Http_header.ContentType "application/json")
    |> Erg.Http_response.set_response_body response)
in
let handlers = Erg.add_handler handler handlers in
Erg.start 8080 handlers
