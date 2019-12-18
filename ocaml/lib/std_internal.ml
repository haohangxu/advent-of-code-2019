open! Core

let filename_param =
  let open Command.Param in
  anon ("filename" %: string)
;;
