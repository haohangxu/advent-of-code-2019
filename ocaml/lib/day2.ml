open! Core
open! Std_internal

let interpret initial_state arg1 arg2 =
  let state = Array.copy initial_state in
  state.(1) <- arg1;
  state.(2) <- arg2;
  let result = Intcode.interpret (Intcode.Program_state.init state ~initial_inputs:[]) in
  result.memory.(0)
;;
  
module Part1 = struct                 
  let command =
    Command.basic
      ~summary:"Part 1 command"
      (Command.Param.map filename_param ~f:(fun filename ->
           (fun () ->
             let input = Stdio.In_channel.read_lines filename in
             assert (List.length input = 1);
             let initial_state = List.hd_exn input |> Intcode.Program_state.Memory.of_string in
             Core.printf "The value at position 0 of the halted state is: %d\n%!" (interpret initial_state 12 2))))
  ;;
end

module Part2 = struct
  let find_inputs (initial_state : int Array.t) (target : int) =
    let all_vals = List.init 100 ~f:Fn.id in
    let possible_inputs = List.cartesian_product all_vals all_vals in
    List.find possible_inputs ~f:(fun (noun, verb) ->
        match Or_error.try_with (fun () -> interpret initial_state noun verb) with
        | Ok result -> result = target
        | Error _ -> false)
    |> Option.value_exn
  ;;
    
  let command =
    let target_param =
      let open Command.Param in
      flag "target" (required int) ~doc:"INT target output"
    in
    Command.basic
      ~summary:"Part 2 command"
      (Command.Param.map2 filename_param target_param ~f:(fun filename target ->
           (fun () ->
             let input = Stdio.In_channel.read_lines filename in
             assert (List.length input = 1);
             let initial_state = List.hd_exn input |> Intcode.Program_state.Memory.of_string in 
             let working_args = find_inputs initial_state target in
             Core.printf !"The desired arguments are: %{sexp: int * int}\n%!" working_args)))
  ;;
end
             
let command =
  Command.group
    ~summary:"Advent of Code 2019 - Day 2"
    [ "1", Part1.command
    ; "2", Part2.command
    ]
;;
