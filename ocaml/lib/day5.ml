open! Core
open! Std_internal
  
module Intcode = struct
  module Mode = struct
    type t =
      | Immediate
      | Position

    let of_int_exn = function
      | 0 -> Position
      | 1 -> Immediate
      | mode -> raise_s [%message "Unknown mode!" (mode : int)]
  end
              
  module Op = struct
    type t =
      | Add of (Mode.t * Mode.t)
      | Multiply of (Mode.t * Mode.t)
      | Input 
      | Output
      | Jump_if_true of (Mode.t * Mode.t)
      | Jump_if_false of (Mode.t * Mode.t)
      | Less_than of (Mode.t * Mode.t)
      | Equals of (Mode.t * Mode.t)
      | Halt
      
    let of_int_exn op =
      let opcode = op % 100 in
      let get_modes op =
        let mode1 = (op / 100) % 10 in
        let mode2 = (op / 1000) % 10 in
        Mode.of_int_exn mode1, Mode.of_int_exn mode2
      in
      match opcode with 
      | 1 -> Add (get_modes op)
      | 2 -> Multiply (get_modes op)
      | 3 -> Input
      | 4 -> Output
      | 5 -> Jump_if_true (get_modes op)
      | 6 -> Jump_if_false (get_modes op)
      | 7 -> Less_than (get_modes op)
      | 8 -> Equals (get_modes op)
      | 99 -> Halt
      | opcode -> raise_s [%message "Unknown opcode!" (opcode : int)]
    ;;
  end

  (* [intepret] takes a singular input, which it uses every time the program
     asks for an input. *)
  let interpret (state : int Array.t) input =
    let rec interpret_next instr_num =
      let get i mode =
        match mode with
        | Mode.Immediate -> state.(i)
        | Position -> state.(state.(i))
      in
      let do_op_and_advance f (mode1, mode2) =
        let arg1 = get (instr_num + 1) mode1 in
        let arg2 = get (instr_num + 2) mode2 in
        let target = get (instr_num + 3) Immediate in
        state.(target) <- f arg1 arg2;
        interpret_next (instr_num + 4)
      in
      let do_input () =
        state.(state.(instr_num + 1)) <- input;
        interpret_next (instr_num + 2)
      in
      let do_output () =
        Core.printf "%d\n%!" (state.(state.(instr_num + 1)));
        interpret_next (instr_num + 2)
      in
      let do_jump_if f (mode1, mode2) =
        let arg1 = get (instr_num + 1) mode1 in
        let arg2 = get (instr_num + 2) mode2 in
        if f arg1
        then interpret_next arg2
        else interpret_next (instr_num + 3)
      in
      match state.(instr_num) |> Op.of_int_exn with
      | Halt -> state
      | Add modes -> do_op_and_advance ( + ) modes
      | Multiply modes -> do_op_and_advance ( * ) modes
      | Input -> do_input ()
      | Output -> do_output ()
      | Jump_if_true modes -> do_jump_if (fun arg -> arg > 0) modes
      | Jump_if_false modes -> do_jump_if (fun arg -> arg = 0) modes
      | Less_than modes -> do_op_and_advance (fun arg1 arg2 -> if arg1 < arg2 then 1 else 0) modes
      | Equals modes -> do_op_and_advance (fun arg1 arg2 -> if arg1 = arg2 then 1 else 0) modes
    in
    interpret_next 0
  ;;

  let state_of_string str =
    String.split str ~on:',' |> List.map ~f:Int.of_string |> Array.of_list 
  ;;
end

module Part1 = struct                 
  let command =
    Command.basic
      ~summary:"Part 1 command"
      (Command.Param.map filename_param ~f:(fun filename ->
           (fun () ->
             let input = Stdio.In_channel.read_lines filename in
             assert (List.length input = 1);
             let initial_state = List.hd_exn input |> Intcode.state_of_string in
             let (_ : int array) = Intcode.interpret initial_state 1 in
             ())))
  ;;
end

module Part2 = struct
  let command =
    Command.basic
      ~summary:"Part 1 command"
      (Command.Param.map filename_param ~f:(fun filename ->
           (fun () ->
             let input = Stdio.In_channel.read_lines filename in
             assert (List.length input = 1);
             let initial_state = List.hd_exn input |> Intcode.state_of_string in
             let (_ : int array) = Intcode.interpret initial_state 5 in
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
