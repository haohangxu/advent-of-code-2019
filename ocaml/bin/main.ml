open! Core
open! Async
open Lib
   
let command =
  Command.group
    ~summary:"Advet of Code 2019 - OCaml solutions"
    [ "day2", Day2.command
    ; "day4", Day4.command
    ]
;;

let () = Command.run command