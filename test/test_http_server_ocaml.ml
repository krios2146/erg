open OUnit2
open Unix

let server_port = 8080

let run_client test_ctxt =
  let server_addr = ADDR_INET (inet_addr_loopback, server_port) in
  let client_socket = socket PF_INET SOCK_STREAM 0 in
  try
    connect client_socket server_addr;
    let message = "Hello, TCP server!" in

    let _ =
      send client_socket (Bytes.of_string message) 0 (String.length message) []
    in

    let buffer = Bytes.create 1024 in
    let bytes_received = recv client_socket buffer 0 1024 [] in

    let response = Bytes.sub_string buffer 0 bytes_received in

    close client_socket;

    assert_equal ~ctxt:test_ctxt message response
  with ex ->
    close client_socket;
    assert_failure ("Client encountered an exception: " ^ Printexc.to_string ex)

let test_tcp_echo_client test_ctxt = run_client test_ctxt

let suite =
  "TCP Client Test" >::: [ "test_tcp_echo_client" >:: test_tcp_echo_client ]

let () =
  let _ = Thread.create Server.run_server server_port in
  run_test_tt_main suite;
  Server.stop_server
