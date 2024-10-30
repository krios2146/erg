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
       ]

let () = run_test_tt_main suite
