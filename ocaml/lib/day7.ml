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

  module Program_state = struct
    module Status = struct
      type t =
        | Never_run of [`initial_input of int]
        | Halted 
        | Blocked_on_input
    end

    type t =
      { memory : int Array.t
      ; instr_num : int
      ; outputs : int list
      ; status : Status.t
      }
  end
                       
  let interpret ?(instr_num = 0) (memory : int Array.t) inputs =
    let rec interpret_next instr_num inputs outputs =
      let get i mode =
        match mode with
        | Mode.Immediate -> memory.(i)
        | Position -> memory.(memory.(i))
      in
      let do_op_and_advance f (mode1, mode2) =
        let arg1 = get (instr_num + 1) mode1 in
        let arg2 = get (instr_num + 2) mode2 in
        let target = get (instr_num + 3) Immediate in
        memory.(target) <- f arg1 arg2;
        interpret_next (instr_num + 4) inputs outputs
      in
      let do_input () =
        match inputs with
        | [] -> Program_state.{ status = Blocked_on_input; memory; instr_num; outputs }
        | hd :: tl ->
           memory.(memory.(instr_num + 1)) <- hd;
           interpret_next (instr_num + 2) tl outputs
      in
      let do_output () =
        interpret_next (instr_num + 2) inputs (memory.(memory.(instr_num + 1)) :: outputs)
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
      match memory.(instr_num) |> Op.of_int_exn with
      | Halt -> { status = Halted; outputs = List.rev outputs; instr_num; memory }
      | Add modes -> do_op_and_advance ( + ) modes
      | Multiply modes -> do_op_and_advance ( * ) modes
      | Input -> do_input ()
      | Output -> do_output ()
      | Jump_if_true modes -> do_jump_if (fun arg -> arg > 0) modes
      | Jump_if_false modes -> do_jump_if (fun arg -> arg = 0) modes
      | Less_than modes -> do_op_and_advance (fun arg1 arg2 -> if arg1 < arg2 then 1 else 0) modes
      | Equals modes -> do_op_and_advance (fun arg1 arg2 -> if arg1 = arg2 then 1 else 0) modes
    in
    interpret_next instr_num inputs []
  ;;

  let state_of_string str =
    String.split str ~on:',' |> List.map ~f:Int.of_string |> Array.of_list 
  ;;
end

module Common = struct
  let ins_all_positions x l =  
    let rec aux prev acc = function
      | [] -> (prev @ [x]) :: acc |> List.rev
      | hd::tl as l -> aux (prev @ [hd]) ((prev @ [x] @ l) :: acc) tl
    in
    aux [] [] l
  ;;
  
  let rec permutations = function
    | [] -> []
    | x::[] -> [[x]]
    | x::xs -> List.fold_left ~init:[] ~f:(fun acc p -> acc @ ins_all_positions x p ) (permutations xs)
  ;;
end
              
module Amplifier = struct
  let run state phases =
    let run_one inputs =
      let state = Array.copy state in
      Intcode.interpret state inputs
    in
    List.fold phases ~init:0 ~f:(fun signal phase -> 
        match run_one [ phase; signal ] with
        | { status = Halted; outputs = [ signal ]; _ } -> signal
        | _ -> raise_s [%message "Unexpected end state!"])
  ;;
end

module Part1 = struct
  let all_permutations = Common.permutations (List.init 5 ~f:Fn.id)
                       
  let find_best_phases state =
    List.map ~f:(Amplifier.run state) all_permutations
    |> List.sort ~compare:(fun i j -> Int.compare j i)
    |> List.hd_exn
  ;;
    
  let command =
    Command.basic
      ~summary:"Part 1 command"
      (Command.Param.map filename_param ~f:(fun filename ->
           (fun () ->
             let input = Stdio.In_channel.read_lines filename in
             assert (List.length input = 1);
             let initial_state = List.hd_exn input |> Intcode.state_of_string in
             Core.printf "Highest signal is %d.\n%!" (find_best_phases initial_state))))
  ;;
end

module Amplifier_v2 = struct
  type t =
    { programs : Intcode.Program_state.t Array.t
    ; current_program : int
    ; total_programs : int
    }

  let create initial_state phases =
    let programs = 
      List.to_array phases
      |> Array.map ~f:(fun phase ->
             { Intcode.Program_state.status = Never_run (`initial_input phase)
             ; outputs = []
             ; memory = Array.copy initial_state
             ; instr_num = 0})
    in
    { programs
    ; current_program = 0
    ; total_programs = List.length phases
    }
  ;;

  let run t =
    let rec run' t inputs = 
      let { Intcode.Program_state.status; outputs = _; memory; instr_num } = t.programs.(t.current_program) in
      let run_with_inputs inputs = 
        let result = Intcode.interpret ~instr_num memory inputs in
         t.programs.(t.current_program) <- result;
         let next_inputs = result.outputs in
         run' { t with current_program = (t.current_program + 1) % t.total_programs } next_inputs
      in
      match status with
      (* If we ever try to run a halted program, exit and return the signal. *)
      | Halted ->
         (match inputs with
          | [ signal ] -> Ok signal
          | _ -> Or_error.error_s [%message "Unexpected end state!"])
      | Blocked_on_input -> run_with_inputs inputs
      | Never_run (`initial_input initial_input) -> run_with_inputs (initial_input :: inputs)
    in
    run' t [ 0 ]
  ;;

  let create_and_run initial_state phases =
    let t = create initial_state phases in
    run t |> Or_error.ok
  ;;
end
                    
module Part2 = struct
  let all_permutations = Common.permutations (List.init 5 ~f:(fun i -> i + 5))
                       
  let find_best_phases memory =
    List.map ~f:(Amplifier_v2.create_and_run memory) all_permutations
    |> List.filter_opt
    |> List.sort ~compare:(fun i j -> Int.compare j i)
    |> List.hd_exn
  ;;
    
  let command =
    Command.basic
      ~summary:"Part 2 command"
      (Command.Param.map filename_param ~f:(fun filename ->
           (fun () ->
             let input = Stdio.In_channel.read_lines filename in
             assert (List.length input = 1);
             let initial_memory = List.hd_exn input |> Intcode.state_of_string in
             Core.printf "Highest signal is %d.\n%!" (find_best_phases initial_memory))))
  ;;
end
                                    
let command =
  Command.group
    ~summary:"Advent of Code 2019 - Day 7"
    [ "1", Part1.command
    ; "2", Part2.command
    ]
;;
