open! Core
open! Std_internal
  
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
    let run_one initial_inputs =
      let state = Array.copy state |> Intcode.Program_state.init ~initial_inputs in
      Intcode.interpret state
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
             let initial_state = List.hd_exn input |> Intcode.Program_state.Memory.of_string in 
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
             Intcode.Program_state.init (Array.copy initial_state) ~initial_inputs:[ phase ])
    in
    { programs
    ; current_program = 0
    ; total_programs = List.length phases
    }
  ;;

  let run t =
    let rec run' t inputs = 
      let program_state = t.programs.(t.current_program) in
      let run_with_inputs inputs = 
        let result = Intcode.interpret { program_state with inputs = program_state.inputs @ inputs } in
        t.programs.(t.current_program) <- { result with outputs = [] };
        run' { t with current_program = (t.current_program + 1) % t.total_programs } result.outputs
      in
      match program_state.status with
      (* If we ever try to run a halted program, exit and return the signal. *)
      | Halted ->
         (match inputs with
          | [ signal ] -> Ok signal
          | _ -> Or_error.error_s [%message "Unexpected end state!"])
      | Blocked_on_input -> run_with_inputs inputs
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
             let initial_memory = List.hd_exn input |> Intcode.Program_state.Memory.of_string in
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
