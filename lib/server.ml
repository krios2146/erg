let accepting_connections = ref true

let create_server_socket port =
  let module U = Unix in
  let socket_address = U.ADDR_INET (U.inet_addr_loopback, port) in
  let socket = U.socket U.PF_INET U.SOCK_STREAM 0 in

  U.setsockopt socket U.SO_REUSEADDR true;
  U.bind socket socket_address;
  U.listen socket 5;

  socket

let handle_client socket =
  let buffer = Bytes.create 1024 in
  let received_bytes = Unix.read socket buffer 0 1024 in

  if received_bytes > 0 then (
    let data = Bytes.sub_string buffer 0 received_bytes in
    Printf.printf "Recieved data: '%s' \n" data;
    ignore (Unix.write socket buffer 0 received_bytes));

  Unix.close socket

let rec accept_connection server_socket =
  let client_socket, client_address = Unix.accept server_socket in
  let client_inet_address =
    match client_address with
    | Unix.ADDR_INET (address, _) -> address
    | _ -> Unix.inet_addr_any
  in
  Printf.printf "Recieved client connection, address: %s \n"
    (Unix.string_of_inet_addr client_inet_address);
  handle_client client_socket;

  if !accepting_connections then accept_connection server_socket
  else Unix.close server_socket

let run_server port =
  let server_socket = create_server_socket port in
  Printf.printf "Server listening on port '%d' \n" port;
  accept_connection server_socket

let stop_server = accepting_connections := false
