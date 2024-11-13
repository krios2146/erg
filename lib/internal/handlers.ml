type t = (string, Handler.t) Hashtbl.t

let add_handler (h : Handler.t) (handlers : t) =
  Hashtbl.add handlers h.uri h;
  handlers
;;

let find_by_uri (handlers : t) uri = Hashtbl.find_opt handlers uri
