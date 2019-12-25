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
      ; relative_base : int 
      } [@@deriving sexp_of]
  end
                       
  let interpret ?(relative_base = 0) ?(instr_num = 0) (memory : int Array.t) inputs =
    let relative_base = ref relative_base in
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
        | [] ->
           { Program_state.status = Blocked_on_input
           ; memory
           ; instr_num
           ; outputs = List.rev outputs
           ; relative_base = !relative_base
           }
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
      | Halt ->
         { status = Halted
         ; outputs = List.rev outputs
         ; instr_num
         ; memory
         ; relative_base = !relative_base
         }
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

module Direction = struct
  type t =
    | Up
    | Down
    | Left
    | Right
  [@@deriving sexp_of]
    
  let move_forward t { Coord.row; col } =
    let row, col =
      match t with
      | Up -> row - 1, col
      | Down -> row + 1, col
      | Left -> row, col - 1
      | Right -> row, col + 1
    in
    { Coord.row; col }
  ;;

  let%expect_test "move_forward" =
    let test t = Core.printf !"%{sexp:Coord.t}\n%!" (move_forward t { Coord.row = 0; col = 0 }) in
    test Up;
    [%expect {| ((row -1) (col 0)) |}];
    test Down;
    [%expect {| ((row 1) (col 0)) |}];
    test Left;
    [%expect {| ((row 0) (col -1)) |}];
    test Right;
    [%expect {| ((row 0) (col 1)) |}]
  ;;
  
  let turn t dir =
    match t with
    | Up -> if dir = 0 then Left else Right
    | Left -> if dir = 0 then Down else Up
    | Down -> if dir = 0 then Right else Left
    | Right -> if dir = 0 then Up else Down
  ;;
end
                 
module Robot = struct
  type t =
    { painted_blocks : int Coord.Table.t
    ; mutable position : Coord.t
    ; mutable direction : Direction.t 
    }
    
  let run_until_halted initial_state handle_output initial_input =
    let rec run_until_blocked ~relative_base ~instr_num memory input = 
      match Intcode.interpret ~relative_base ~instr_num memory input with
      | { status = Blocked_on_input; outputs; instr_num; memory; relative_base } ->
         let input = handle_output outputs in
         run_until_blocked ~relative_base ~instr_num memory input
      | { status = Halted; outputs; _ } -> handle_output outputs |> ignore
    in
    run_until_blocked ~relative_base:0 ~instr_num:0 initial_state initial_input
  ;;

  let init_and_run initial_state initial_input =
    let t = { painted_blocks = Coord.Table.create (); position = { Coord.row = 0; col = 0 }; direction = Direction.Up } in
    let current_color () =
      Coord.Table.find t.painted_blocks t.position |> Option.value ~default:0
    in
    let handle_output = function
      | [ color; direction_to_turn ] ->
         Coord.Table.set t.painted_blocks ~key:t.position ~data:color;
         t.direction <- Direction.turn t.direction direction_to_turn;
         t.position <- Direction.move_forward t.direction t.position;
         [ current_color () ]
      | _ -> assert false
    in
    run_until_halted initial_state handle_output initial_input;
    t.painted_blocks
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
             let painted_blocks = Robot.init_and_run initial_state [ 0 ] in
             Core.printf "We painted %d blocks.\n%!" (Coord.Table.keys painted_blocks |> List.length))))
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
             let painted_blocks = Robot.init_and_run initial_state [ 1 ] in
             let smallest, largest =
               Coord.Table.fold
                 painted_blocks
                 ~init:({ Coord.row = 0; col = 0 }, { Coord.row = 0; col = 0 })
                 ~f:(fun ~key:coord ~data:color (smallest, largest) ->
                   if color = 1
                   then
                     { row = Int.min smallest.row coord.row; col = Int.min smallest.col coord.col },
                     { row = Int.max largest.row coord.row; col = Int.max largest.col coord.col }
                   else smallest, largest)
             in
             List.iter (List.range smallest.row (largest.row + 1)) ~f:(fun row ->
                 List.iter (List.range smallest.col (largest.col + 1)) ~f:(fun col ->
                     match Coord.Table.find painted_blocks { row; col } with
                     | None | Some 0 -> Core.printf " %!"
                     | _ -> Core.printf "#%!");
                 Core.printf "\n%!")
           )))
  ;;
end
                                    
let command =
  Command.group
    ~summary:"Advent of Code 2019 - Day 9"
    [ "1", Part1.command
    ; "2", Part2.command
    ]
;;
