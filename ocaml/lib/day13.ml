open! Core
open! Std_internal

  
module Brick_breaker = struct
  let num_rows = 24
  let num_cols = 45
               
  type t =
    { board : string Array.t Array.t
    ; mutable score : int
    ; mutable joystick : int
    ; game_over : bool ref
    }

  let init () =
    Graphics.open_graph
      (Printf.sprintf " %dx%d" num_rows num_cols);
    let board = Array.make_matrix ~dimx:num_rows ~dimy:num_cols " " in 
   { board; score = 0; joystick = 0; game_over = ref false }
  ;;
    
  let update_from_program_state t program_state =
    let objects = List.chunks_of program_state.Intcode.Program_state.outputs ~length:3 in
    List.iter objects ~f:(function
        | [ -1; 0; score ] ->
           t.score <- score
        | [ col; row; item ] ->
           let elt = 
             match item with
             | 1 -> "#"
             | 2 -> "="
             | 3 -> "_"
             | 4 -> "o"
             | _ -> " "
           in
           t.board.(row).(col) <- elt
        | _ -> raise_s [%message "Unexpected output!"])
  ;;

  let render t =
    Array.map t.board ~f:(fun row -> 
        String.concat_array ~sep:"" row)
    |> String.concat_array ~sep:"\n"
    |> Core.print_endline;
    Core.printf "Score: %d\n%!" t.score
  ;;

  let read_key () =
    if Graphics.key_pressed ()
    then Some (Graphics.read_key ())
    else None
  ;;

  let every seconds ~f ~stop =
    let open Async in
    let open Core in
    let rec loop () =
      if !stop
      then return ()
      else
        Clock.after (Time.Span.of_sec seconds)
        >>= fun () ->
        f ();
        loop ()
    in
    don't_wait_for (loop ())
  ;;

  let listen_for_joystick t = 
    every ~stop:t.game_over 0.01 ~f:(fun () ->
        match read_key () with
        | None -> t.joystick <- 0
        | Some key ->
           match key with
           | 'a' -> t.joystick <- -1
           | 'd' -> t.joystick <- 1
           | _ -> ())
  ;;
  
  let run initial_state =
    let t = init () in
    listen_for_joystick t;
    let rec run' program =
      let result = Intcode.interpret program in
      update_from_program_state t result;
      render t;
      match result.Intcode.Program_state.status with
      | Halted -> t.game_over := true
      | Blocked_on_input -> 
         run' { result with inputs = [ t.joystick ] } 
    in
    run' initial_state
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
               |> Intcode.Program_state.init ~initial_inputs:[]
             in
             let brick_breaker = Brick_breaker.init () in
             let result = Intcode.interpret initial_state in
             Brick_breaker.update_from_program_state brick_breaker result;
             Brick_breaker.render brick_breaker;
             let board = List.chunks_of result.outputs ~length:3 in
             let num_rows, num_cols =
               List.fold board ~init:(0, 0) ~f:(fun (max_row, max_col) next ->
                   match next with
                   | [ col; row; _ ] -> Int.max max_row row, Int.max max_col col
                   | _ -> assert false)
             in
             List.map board ~f:List.last_exn
             |> List.count ~f:(( = ) 2)
             |> Core.printf "The dimensions of the board are %dx%d. The number of blocks is %d\n%!" (num_rows + 1) (num_cols + 1);
      )))
  ;;
end
                     
module Part2 = struct
  let command =
    Async.Command.async
      ~summary:"Part 1 command"
      (Command.Param.map filename_param ~f:(fun filename ->
           (fun () ->
             let input = Stdio.In_channel.read_lines filename in
             let initial_state =
               List.hd_exn input
               |> Intcode.Program_state.Memory.of_string
               |> Intcode.Program_state.init ~initial_inputs:[]
             in
             Brick_breaker.run initial_state;
             Async.Deferred.never ())))
  ;;
end
                                    
let command =
  Command.group
    ~summary:"Advent of Code 2019 - Day 9"
    [ "1", Part1.command
    ; "2", Part2.command
    ]
;;
