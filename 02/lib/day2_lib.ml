open! Core

module Common = struct
  let filename_param =
    let open Command.Param in
    anon ("filename" %: string)
  ;;
end

module Intcode = struct
  module Op = struct
    type t =
      | Add
      | Multiply
      | Halt
      
    let of_int_exn = function
      | 1 -> Add
      | 2 -> Multiply
      | 99 -> Halt
      | opcode -> raise_s [%message "Unknown opcode!" (opcode : int)]
    ;;
  end

  let interpret' (state : int Array.t) =
    let rec interpret_next instr_num =
      let do_op_and_advance f =
        let arg1 = state.(state.(instr_num + 1)) in
        let arg2 = state.(state.(instr_num + 2)) in
        let target = state.(instr_num + 3) in
        state.(target) <- f arg1 arg2;
        interpret_next (instr_num + 4)
      in
      match state.(instr_num) |> Op.of_int_exn with
      | Halt -> state
      | Add -> do_op_and_advance ( + )
      | Multiply -> do_op_and_advance ( * )
    in
    interpret_next 0
  ;;

  let state_of_string str =
    String.split str ~on:',' |> List.map ~f:Int.of_string |> Array.of_list 
  ;;
  
  let%expect_test "interpret" =
    let test str = printf !"%{sexp: int Array.t}" (interpret' (state_of_string str)) in
    test "1,9,10,3,2,3,11,0,99,30,40,50";
    [%expect {| (3500 9 10 70 2 3 11 0 99 30 40 50) |}];
    test "1,0,0,0,99";
    [%expect {| (2 0 0 0 99) |}];
    test "2,3,0,3,99";
    [%expect {| (2 3 0 6 99) |}];
    test "2,4,4,5,99,0";
    [%expect {| (2 4 4 5 99 9801) |}];
    test "1,1,1,4,99,5,6,0,99";
    [%expect {| (30 1 1 4 2 5 6 0 99) |}]
  ;;

  let interpret (state : int Array.t) (arg1 : int) (arg2 : int) =
    state.(1) <- arg1;
    state.(2) <- arg2;
    (interpret' state).(0)
  ;;
end

module Part1 = struct                 
  let command =
    Command.basic
      ~summary:"Part 1 command"
      (Command.Param.map Common.filename_param ~f:(fun filename ->
           (fun () ->
             let input = Stdio.In_channel.read_lines filename in
             assert (List.length input = 1);
             let initial_state = List.hd_exn input |> Intcode.state_of_string in
             let output = Intcode.interpret initial_state 12 2 in
             Core.printf "The value at position 0 of the halted state is: %d\n%!" output)))
  ;;
end

module Part2 = struct
  let find_inputs (initial_state : int Array.t) (target : int) =
    let all_vals = List.init 100 ~f:Fn.id in
    let possible_inputs = List.cartesian_product all_vals all_vals in
    List.find possible_inputs ~f:(fun (noun, verb) ->
        let state = Array.copy initial_state in
        match Or_error.try_with (fun () -> Intcode.interpret state noun verb) with
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
      ~summary:"Part 1 command"
      (Command.Param.map2 Common.filename_param target_param ~f:(fun filename target ->
           (fun () ->
             let input = Stdio.In_channel.read_lines filename in
             assert (List.length input = 1);
             let initial_state = List.hd_exn input |> Intcode.state_of_string in
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
