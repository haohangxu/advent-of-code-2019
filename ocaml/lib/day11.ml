open! Core
open! Std_internal
  
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
    
  let run_until_halted initial_state handle_output initial_inputs =
    let program_state = Intcode.Program_state.init initial_state ~initial_inputs in
    let rec run_until_blocked program_state = 
      match Intcode.interpret program_state with
      | { Intcode.Program_state.status = Blocked_on_input; outputs; _ } as program_state ->
         let inputs = handle_output outputs in
         run_until_blocked { program_state with inputs; outputs = [] }
      | { status = Halted; outputs; _ } -> handle_output outputs |> ignore
    in
    run_until_blocked program_state
  ;;

  let init_and_run initial_state initial_input =
    let t =
      { painted_blocks = Coord.Table.create ()
      ; position = { Coord.row = 0; col = 0 }
      ; direction = Direction.Up }
    in
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
             let initial_state =
               List.hd_exn input
               |> Intcode.Program_state.Memory.of_string
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
             let initial_state =
               List.hd_exn input
               |> Intcode.Program_state.Memory.of_string
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
