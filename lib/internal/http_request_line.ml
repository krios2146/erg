type t =
  { http_method : Http_method.t
  ; request_uri : string
  ; http_version : string
  ; query : Query.t
  }

let rec parse_query uri =
  match String.split_on_char '?' uri with
  | [ _; query ] ->
    query
    |> url_decode
    |> String.split_on_char '&'
    |> List.filter (fun s -> String.length s > 0)
    |> List.map (String.split_on_char '=')
    |> List.filter (fun k_v_pair -> List.length k_v_pair = 2)
    |> List.fold_left add_pair_to_query (Query.empty ())
  | _ -> Query.empty ()

and add_pair_to_query q k_v_pair =
  let key = List.nth k_v_pair 0 in
  let value = List.nth k_v_pair 1 in
  Query.add key value q

and url_decode data =
  let data_len = String.length data in
  let buffer = Buffer.create data_len in
  let rec aux i =
    if i >= data_len
    then Buffer.contents buffer
    else (
      match data.[i] with
      | '%' when i + 2 < data_len ->
        let hex = String.sub data (i + 1) 2 in
        let char_code = "0x" ^ hex |> int_of_string in
        Buffer.add_char buffer (char_of_int char_code);
        aux (i + 3)
      | '+' ->
        Buffer.add_char buffer ' ';
        aux (i + 1)
      | c ->
        Buffer.add_char buffer c;
        aux (i + 1))
  in
  aux 0
;;

let from_string req_line =
  let req_line_parts = String.split_on_char Http_notation.sp req_line in
  let open Http_error in
  match req_line_parts with
  | [] -> Error Malformed_request_line
  | [ _; _ ] -> Error Malformed_request_line
  | [ http_method; uri; version ] ->
    let http_method = Http_method.from_string http_method in
    (match http_method with
     | Ok http_method ->
       let query = parse_query uri in
       Ok { http_method; request_uri = uri; http_version = version; query }
     | Error e -> Error e)
  | _ -> Error Malformed_request_line
;;

let pretty_print request_line =
  Printf.sprintf
    "HTTP Method: %s \nRequest URI: %s \nHTTP Version: %s \n"
    (Http_method.to_string request_line.http_method)
    request_line.request_uri
    request_line.http_version
;;
