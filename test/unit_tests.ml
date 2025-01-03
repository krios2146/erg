open OUnit2
open Erg_internal

let parse_http_request_test_valid_request test_ctxt =
  let data =
    "POST /currencies HTTP/1.1\r\n\
     User-Agent: PostmanRuntime/7.42.0\r\n\
     Accept: */*\r\n\
     Postman-Token: 67d4cad6-72db-465c-aa4b-c71c67254551\r\n\
     Host: localhost:8080\r\n\
     Accept-Encoding: gzip, deflate, br\r\n\
     Connection: keep-alive\r\n\
     Content-Type: application/x-www-form-urlencoded\r\n\
     Content-Length: 41\r\n\
     \r\n\
     name=Czech%20Koruna&code=CZK&sign=K%C4%8D"
  in
  let open Http_request in
  let expected =
    Ok
      { request_line =
          { http_method = Post
          ; request_uri = "/currencies"
          ; http_version = "HTTP/1.1"
          ; query = Query.empty ()
          }
      ; headers =
          [ UserAgent "PostmanRuntime/7.42.0"
          ; Accept "*/*"
          ; AcceptEncoding "gzip, deflate, br"
          ; Connection "keep-alive"
          ; ContentType "application/x-www-form-urlencoded"
          ; ContentLength "41"
          ]
      ; message_body = "name=Czech%20Koruna&code=CZK&sign=K%C4%8D"
      }
  in
  let result = Http_request.from_string data in
  assert_equal
    ~ctxt:test_ctxt
    ~printer:(fun r ->
      match r with
      | Error e -> Http_error.to_string e
      | Ok r -> Http_request.pretty_print r)
    expected
    result
;;

let parse_http_request_test_no_headers test_ctxt =
  let data =
    "POST /currencies HTTP/1.1\r\n\r\nname=Czech%20Koruna&code=CZK&sign=K%C4%8D"
  in
  let expected =
    let open Http_request in
    Ok
      { request_line =
          { http_method = Post
          ; request_uri = "/currencies"
          ; http_version = "HTTP/1.1"
          ; query = Query.empty ()
          }
      ; headers = []
      ; message_body = "name=Czech%20Koruna&code=CZK&sign=K%C4%8D"
      }
  in
  let result = Http_request.from_string data in
  assert_equal
    ~ctxt:test_ctxt
    ~printer:(fun r ->
      match r with
      | Error e -> Http_error.to_string e
      | Ok r -> Http_request.pretty_print r)
    expected
    result
;;

let parse_http_request_test_missing_request_line test_ctxt =
  let data =
    "User-Agent: PostmanRuntime/7.42.0\r\n\
     Accept: */*\r\n\
     Postman-Token: 67d4cad6-72db-465c-aa4b-c71c67254551\r\n\
     Host: localhost:8080\r\n\
     Accept-Encoding: gzip, deflate, br\r\n\
     Connection: keep-alive\r\n\
     Content-Type: application/x-www-form-urlencoded\r\n\
     Content-Length: 41\r\n\
     \r\n\
     name=Czech%20Koruna&code=CZK&sign=K%C4%8D"
  in
  let expected = Error Http_error.Malformed_request_line in
  let result = Http_request.from_string data in
  assert_equal
    ~ctxt:test_ctxt
    ~printer:(fun r ->
      match r with
      | Error e -> Http_error.to_string e
      | Ok r -> Http_request.pretty_print r)
    expected
    result
;;

let suite =
  "Server internals test"
  >::: [ "parse_http_request_test_valid_request" >:: parse_http_request_test_valid_request
       ; "parse_http_request_test_no_headers" >:: parse_http_request_test_no_headers
       ; "parse_http_request_test_missing_request_line"
         >:: parse_http_request_test_missing_request_line
       ]
;;

let () = run_test_tt_main suite
