type t =
  { http_method : Http_method.t
  ; uri : string
  ; handler_func : Http_request.t -> Http_response.t
  }

let create_handler http_method uri handler_func = { http_method; uri; handler_func }
