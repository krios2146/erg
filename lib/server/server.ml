let accepting_connections = ref true

let create_server_socket port =
  try
    let module U = Unix in
    let socket_address = U.ADDR_INET (U.inet_addr_loopback, port) in
    let socket = U.socket U.PF_INET U.SOCK_STREAM 0 in

    U.setsockopt socket U.SO_REUSEADDR true;
    U.bind socket socket_address;
    U.listen socket 5;

    Some socket
  with
  | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
      Log.error (fun m ->
          m "Unable to bind TCP socket, port %d already in use" port);
      None
  | _ ->
      Log.error (fun m ->
          m "Unknown error occured while creating server socket on port %d" port);
      None

let handle_tcp_client_data data =
  Log.debug (fun m -> m "Recieved data:\n%s" data);
  data

let handle_client socket =
  let buffer = Bytes.create 1024 in
  let received_bytes = Unix.read socket buffer 0 1024 in

  (if received_bytes > 0 then
     let data = Bytes.sub_string buffer 0 received_bytes in
     let handeled_data = handle_tcp_client_data data in
     let handeled_data_bytes = Bytes.of_string handeled_data in
     ignore
       (Unix.write socket handeled_data_bytes 0
          (Bytes.length handeled_data_bytes)));

  Unix.close socket

let accept_connection server_socket =
  Log.debug (fun m -> m "Accepting connections");

  (* Not sure whether doing it recursively is a good idea, so there is a loop *)
  while !accepting_connections do
    let client_socket, client_address = Unix.accept server_socket in

    let client_inet_address =
      match client_address with
      | Unix.ADDR_INET (address, _) -> address
      | _ -> Unix.inet_addr_any
    in
    let client_inet_address_str =
      Unix.string_of_inet_addr client_inet_address
    in
    Log.debug (fun m ->
        m "Recieved client connection, address: %s" client_inet_address_str);

    handle_client client_socket
  done;

  Unix.close server_socket

let run_server port =
  let server_socket = create_server_socket port in
  match server_socket with
  | Some socket ->
      Log.info (fun m -> m "Server listening on port %d" port);
      accept_connection socket
  | None -> ()

let stop_server () = accepting_connections := false
