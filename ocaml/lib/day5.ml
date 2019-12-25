open! Core
open! Std_internal
  
module Part1 = struct                 
  let command =
    Command.basic
      ~summary:"Part 1 command"
      (Command.Param.map filename_param ~f:(fun filename ->
           (fun () ->
             let input = Stdio.In_channel.read_lines filename in
             assert (List.length input = 1);
             let initial_state =
               List.hd_exn input |> Intcode.Program_state.Memory.of_string
               |> Intcode.Program_state.init ~initial_inputs:[ 1 ] 
             in
             let result = Intcode.interpret initial_state in
             Core.printf !"%{sexp: int list}\n%!" result.outputs;
             ())))
  ;;
end

module Part2 = struct
  let command =
    Command.basic
      ~summary:"Part 2 command"
      (Command.Param.map filename_param ~f:(fun filename ->
           (fun () ->
             let input = Stdio.In_channel.read_lines filename in
             assert (List.length input = 1);
             let initial_state =
               List.hd_exn input |> Intcode.Program_state.Memory.of_string
               |> Intcode.Program_state.init ~initial_inputs:[ 5 ] 
             in
             let result = Intcode.interpret initial_state in
             Core.printf !"%{sexp: int list}\n%!" result.outputs;
             ())))
  ;;
end
             
let command =
  Command.group
    ~summary:"Advent of Code 2019 - Day 5"
    [ "1", Part1.command
    ; "2", Part2.command
    ]
;;
