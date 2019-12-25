open! Core
open! Async
open Lib
   
let command =
  Command.group
    ~summary:"Advet of Code 2019 - OCaml solutions"
    [ "day2", Day2.command
    ; "day4", Day4.command
    ; "day5", Day5.command
    ; "day6", Day6.command
    ; "day7", Day7.command
    ; "day8", Day8.command
    ; "day9", Day9.command
    ; "day10", Day10.command
    ; "day11", Day11.command
    ; "day12", Day12.command
    ]
;;

let () = Command.run command
