open OUnit2

let split_on_first_test_request_line_to_headers test_ctxt =
  let data =
    "GET /currencies HTTP/1.1\r\n\
     User-Agent: PostmanRuntime/7.42.0\r\n\
     \r\n\
     message"
  in

  let expected =
    Some
      ( "GET /currencies HTTP/1.1",
        "User-Agent: PostmanRuntime/7.42.0\r\n\r\nmessage" )
  in

  let result = Server.split_on_first "\r\n" data in

  assert_equal ~ctxt:test_ctxt expected result

let split_on_first_test_headers_to_crlf test_ctxt =
  let data = "User-Agent: PostmanRuntime/7.42.0\r\n\r\nmessage" in

  let expected = Some ("User-Agent: PostmanRuntime/7.42.0", "\r\nmessage") in

  let result = Server.split_on_first "\r\n" data in

  assert_equal ~ctxt:test_ctxt expected result

let split_on_first_test_crlf_to_message test_ctxt =
  let data = "\r\nmessage" in

  let expected = Some ("", "message") in

  let result = Server.split_on_first "\r\n" data in

  assert_equal ~ctxt:test_ctxt expected result

let split_on_first_test_request_line_to_crlf test_ctxt =
  let data = "GET /currencies HTTP/1.1\r\n\r\nmessage" in

  let expected = Some ("GET /currencies HTTP/1.1", "\r\nmessage") in

  let result = Server.split_on_first "\r\n" data in

  assert_equal ~ctxt:test_ctxt expected result

let parse_http_request_test_valid_request test_ctxt =
  let open Server in
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

  let expected =
    Ok
      {
        request_line =
          {
            http_method = POST;
            request_uri = "/currencies";
            http_version = "HTTP/1.1";
          };
        headers =
          [
            RequestHeader (UserAgent "PostmanRuntime/7.42.0");
            RequestHeader (Accept "*/*");
            RequestHeader (AcceptEncoding "gzip, deflate, br");
            GeneralHeader (Connection "keep-alive");
            EntityHeader (ContentType "application/x-www-form-urlencoded");
            EntityHeader (ContentLength "41");
          ];
        message_body = "name=Czech%20Koruna&code=CZK&sign=K%C4%8D";
      }
  in

  let result = Server.parse_http_request data in

  assert_equal ~ctxt:test_ctxt
    ~printer:(fun r ->
      match r with
      | Error e -> error_to_string e
      | Ok r -> http_request_to_string r)
    expected result

let parse_http_request_test_no_headers test_ctxt =
  let open Server in
  let data =
    "POST /currencies HTTP/1.1\r\n\r\nname=Czech%20Koruna&code=CZK&sign=K%C4%8D"
  in

  let expected =
    Ok
      {
        request_line =
          {
            http_method = POST;
            request_uri = "/currencies";
            http_version = "HTTP/1.1";
          };
        headers = [];
        message_body = "name=Czech%20Koruna&code=CZK&sign=K%C4%8D";
      }
  in

  let result = Server.parse_http_request data in

  assert_equal ~ctxt:test_ctxt
    ~printer:(fun r ->
      match r with
      | Error e -> error_to_string e
      | Ok r -> http_request_to_string r)
    expected result

let parse_http_request_test_missing_request_line test_ctxt =
  let open Server in
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

  let expected = Error Malformed_request_line in

  let result = Server.parse_http_request data in

  assert_equal ~ctxt:test_ctxt
    ~printer:(fun r ->
      match r with
      | Error e -> error_to_string e
      | Ok r -> http_request_to_string r)
    expected result

let suite =
  "Server internals test"
  >::: [
         "split_on_first_test_request_line_to_headers"
         >:: split_on_first_test_request_line_to_headers;
         "split_on_first_test_headers_to_crlf"
         >:: split_on_first_test_headers_to_crlf;
         "split_on_first_test_crlf_to_message"
         >:: split_on_first_test_crlf_to_message;
         "split_on_first_test_request_line_to_crlf"
         >:: split_on_first_test_request_line_to_crlf;
         "parse_http_request_test_valid_request"
         >:: parse_http_request_test_valid_request;
         "parse_http_request_test_no_headers"
         >:: parse_http_request_test_no_headers;
         "parse_http_request_test_missing_request_line"
         >:: parse_http_request_test_missing_request_line;
       ]

let () = run_test_tt_main suite
