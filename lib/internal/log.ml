let erg_logs_src = Logs.Src.create "Erg" ~doc:"Erg is a library that implements HTTP/1.1"
let log_module = Logs.src_log erg_logs_src

let info message =
  let (module Log) = log_module in
  Log.info message
;;

let error message =
  let (module Log) = log_module in
  Log.err message
;;

let warn message =
  let (module Log) = log_module in
  Log.warn message
;;

let debug message =
  let (module Log) = log_module in
  Log.debug message
;;
