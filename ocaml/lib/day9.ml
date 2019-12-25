open! Core
open! Std_internal
  
module Intcode = struct
  module Mode = struct
    type t =
      | Position
      | Relative
      | Immediate      
    [@@deriving sexp_of]
      
    let of_int_exn = function
      | 0 -> Position
      | 1 -> Immediate
      | 2 -> Relative
      | mode -> raise_s [%message "Unknown mode!" (mode : int)]

    module Positional = struct
      type t =
        | Position
        | Relative
      [@@deriving sexp_of]
        
      let of_int_exn i =
        match of_int_exn i with
        | Immediate -> raise_s [%message "Weird! Got Immediate when expecting positional mode"]
        | Position -> Position
        | Relative -> Relative
    end
  end
              
  module Op = struct
    type t =
      | Add of (Mode.t * Mode.t * Mode.Positional.t)
      | Multiply of (Mode.t * Mode.t * Mode.Positional.t)
      | Input of Mode.Positional.t
      | Output of Mode.t
      | Jump_if_true of (Mode.t * Mode.t)
      | Jump_if_false of (Mode.t * Mode.t)
      | Less_than of (Mode.t * Mode.t * Mode.Positional.t)
      | Equals of (Mode.t * Mode.t * Mode.Positional.t)
      | Adjust_relative_base of Mode.t
      | Halt
    [@@deriving sexp_of]
      
    let of_int_exn op =
      let opcode = op % 100 in
      let get_modes_2 op =
        let mode1 = (op / 100) % 10 in
        let mode2 = (op / 1000) % 10 in
        Mode.of_int_exn mode1, Mode.of_int_exn mode2
      in
      let get_modes op =
        let mode1, mode2 = get_modes_2 op in 
        let mode3 = (op / 10000) % 10 in
        mode1, mode2, Mode.Positional.of_int_exn mode3
      in
      let get_positional_mode op =
        let mode = (op / 100) % 10 in
        Mode.Positional.of_int_exn mode
      in
      let get_mode op =
        let mode = (op / 100) % 10 in
        Mode.of_int_exn mode
      in
      match opcode with 
      | 1 -> Add (get_modes op)
      | 2 -> Multiply (get_modes op)
      | 3 -> Input (get_positional_mode op)
      | 4 -> Output (get_mode op)
      | 5 -> Jump_if_true (get_modes_2 op)
      | 6 -> Jump_if_false (get_modes_2 op)
      | 7 -> Less_than (get_modes op)
      | 8 -> Equals (get_modes op)
      | 9 -> Adjust_relative_base (get_mode op)
      | 99 -> Halt
      | opcode -> raise_s [%message "Unknown opcode!" (opcode : int)]
    ;;
  end

  module Program_state = struct
    module Status = struct
      type t =
        | Halted 
        | Blocked_on_input
        [@@deriving sexp_of]
    end

    type t =
      { memory : int Array.t
      ; instr_num : int
      ; outputs : int list
      ; status : Status.t
      } [@@deriving sexp_of]
  end
                       
  let interpret ?(instr_num = 0) (memory : int Array.t) inputs =
    let relative_base = ref 0 in
    let rec interpret_next instr_num inputs outputs =
      let get i mode =
        match mode with
        | Mode.Immediate -> memory.(i)
        | Position -> memory.(memory.(i))
        | Relative -> memory.(!relative_base + memory.(i))
      in
      let get_i i mode =
        match mode with
        | Mode.Positional.Position -> memory.(i)
        | Relative -> !relative_base + memory.(i)
      in
      let do_op_and_advance f (mode1, mode2, mode3) =
        let arg1 = get (instr_num + 1) mode1 in
        let arg2 = get (instr_num + 2) mode2 in
        let target = get_i (instr_num + 3) mode3 in
        memory.(target) <- f arg1 arg2;
        interpret_next (instr_num + 4) inputs outputs
      in
      let do_input mode =
        match inputs with
        | [] -> Program_state.{ status = Blocked_on_input; memory; instr_num; outputs }
        | hd :: tl ->
           let arg_i = get_i (instr_num + 1) mode in
           memory.(arg_i) <- hd;
           interpret_next (instr_num + 2) tl outputs
      in
      let do_output mode =
        let arg = get (instr_num + 1) mode in 
        interpret_next (instr_num + 2) inputs (arg :: outputs)
      in
      let do_jump_if f (mode1, mode2) =
        let arg1 = get (instr_num + 1) mode1 in
        let arg2 = get (instr_num + 2) mode2 in
        let next_instr = 
          if f arg1
          then arg2
          else instr_num + 3
        in
        interpret_next next_instr inputs outputs
      in
      let do_base_adjust mode =
        let arg = get (instr_num + 1) mode in
        relative_base := !relative_base + arg;
        interpret_next (instr_num + 2) inputs outputs
      in
      match memory.(instr_num) |> Op.of_int_exn with
      | Halt -> { status = Halted; outputs = List.rev outputs; instr_num; memory }
      | Add modes -> do_op_and_advance ( + ) modes
      | Multiply modes -> do_op_and_advance ( * ) modes
      | Input mode -> do_input mode
      | Output mode -> do_output mode
      | Jump_if_true modes -> do_jump_if (fun arg -> arg > 0) modes
      | Jump_if_false modes -> do_jump_if (fun arg -> arg = 0) modes
      | Less_than modes -> do_op_and_advance (fun arg1 arg2 -> if arg1 < arg2 then 1 else 0) modes
      | Equals modes -> do_op_and_advance (fun arg1 arg2 -> if arg1 = arg2 then 1 else 0) modes
      | Adjust_relative_base mode -> do_base_adjust mode
    in
    interpret_next instr_num inputs []
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
             (* Extend the system memory by a lot. *)
             let initial_state =
               Array.append initial_state (Array.init 1000 ~f:(const 0))
             in
             match Intcode.interpret initial_state [ 1 ] with
             | { status = Halted; outputs; _ } -> Core.printf !"%{sexp: int list}\n%!" outputs
             | state -> raise_s [%message "Unexpected end state!" (state : Intcode.Program_state.t)])))
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
             (* Extend the system memory by a lot. *)
             let initial_state =
               Array.append initial_state (Array.init 1000 ~f:(const 0))
             in
             match Intcode.interpret initial_state [ 2 ] with
             | { status = Halted; outputs; _ } -> Core.printf !"%{sexp: int list}\n%!" outputs
             | state -> raise_s [%message "Unexpected end state!" (state : Intcode.Program_state.t)])))
  ;;
end
                                    
let command =
  Command.group
    ~summary:"Advent of Code 2019 - Day 9"
    [ "1", Part1.command
    ; "2", Part2.command
    ]
;;
