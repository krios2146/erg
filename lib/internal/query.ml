type t = (string, string) Hashtbl.t

let get param (query : t) = Hashtbl.find_opt query param
let empty () : t = Hashtbl.create 10

let add key value q =
  Hashtbl.add q key value;
  q
;;
