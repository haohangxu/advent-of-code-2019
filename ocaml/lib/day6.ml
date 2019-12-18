open! Core
open! Std_internal

module Relationship = struct
  type t =
    { out : string
    ; in_ : string
    }

  let of_string_exn s =
    match String.split s ~on:')' with
    | [] | [ _ ] | _ :: _ :: _ :: _ -> raise_s [%message "Unparsable input" (s : string)]
    | [ in_ ; out ] -> { in_; out }
end
                    
module Orbit_tracker = struct
  type t =
    { inner_things : (string * int) list String.Map.t
    ; outer_things : (string * int) list String.Map.t
    }
  [@@deriving sexp, compare]

  let canonicalize t =
    let sort =
      String.Map.map ~f:(List.sort ~compare:(Comparable.lift String.compare ~f:fst))
    in
    { inner_things = sort t.inner_things; outer_things = sort t.outer_things }
  ;;
    
  let empty = { inner_things = String.Map.empty; outer_things = String.Map.empty } 

  let process_new_relationship
        { inner_things; outer_things }
        { Relationship.in_; out } =
    let all_things_more_outer_than_out = (out, 0) :: Option.value ~default:[] (String.Map.find outer_things out) in
    let all_things_more_inner_than_in_ = (in_, 0) :: Option.value ~default:[] (String.Map.find inner_things in_) in
    let outer_things =
      List.fold ~init:outer_things all_things_more_inner_than_in_ ~f:(fun accum (next, dist) ->
          let new_things =
            List.map all_things_more_outer_than_out
              ~f:(fun (thing, dist') -> (thing, dist + dist' + 1))
          in
          String.Map.change accum next ~f:(function
              | None -> Some new_things
              | Some existing -> Some (existing @ new_things)))
    in
    let inner_things =
      List.fold ~init:inner_things all_things_more_outer_than_out ~f:(fun accum (next, dist) ->
          let new_things =
            List.map all_things_more_inner_than_in_
              ~f:(fun (thing, dist') -> (thing, dist + dist' + 1))
          in
          String.Map.change accum next ~f:(function
              | None -> Some new_things
              | Some existing -> Some (existing @ new_things)))
    in
    { inner_things; outer_things }
  ;;
end

module Common = struct
  let process_input input =
    List.fold input ~init:Orbit_tracker.empty ~f:(fun accum rel ->
        Orbit_tracker.process_new_relationship accum (Relationship.of_string_exn rel))
  ;;

  let%expect_test "process_input" =
    let input =
      [ "COM)B"
      ; "B)C"
      ; "C)D"
      ; "D)E"
      ; "E)F"
      ; "B)G"
      ; "G)H"
      ; "D)I"
      ; "E)J"
      ; "J)K"
      ; "K)L"
      ]
    in
    let processed = Orbit_tracker.canonicalize (process_input input) in
    let processed' = Orbit_tracker.canonicalize (process_input (List.permute input)) in
    assert ([%compare.equal: Orbit_tracker.t] processed processed');
    Core.printf !"%{sexp: Orbit_tracker.t}\n%!" processed;
    [%expect {|
      ((inner_things
        ((B ((COM 1))) (C ((B 1) (COM 2))) (D ((B 2) (C 1) (COM 3)))
         (E ((B 3) (C 2) (COM 4) (D 1))) (F ((B 4) (C 3) (COM 5) (D 2) (E 1)))
         (G ((B 1) (COM 2))) (H ((B 2) (COM 3) (G 1)))
         (I ((B 3) (C 2) (COM 4) (D 1))) (J ((B 4) (C 3) (COM 5) (D 2) (E 1)))
         (K ((B 5) (C 4) (COM 6) (D 3) (E 2) (J 1)))
         (L ((B 6) (C 5) (COM 7) (D 4) (E 3) (J 2) (K 1)))))
       (outer_things
        ((B ((C 1) (D 2) (E 3) (F 4) (G 1) (H 2) (I 3) (J 4) (K 5) (L 6)))
         (C ((D 1) (E 2) (F 3) (I 2) (J 3) (K 4) (L 5)))
         (COM ((B 1) (C 2) (D 3) (E 4) (F 5) (G 2) (H 3) (I 4) (J 5) (K 6) (L 7)))
         (D ((E 1) (F 2) (I 1) (J 2) (K 3) (L 4))) (E ((F 1) (J 1) (K 2) (L 3)))
         (G ((H 1))) (J ((K 1) (L 2))) (K ((L 1)))))) |}]
  ;;
end
              
module Part1 = struct
  let find_all_outers input =
    let orbit_tracker = Common.process_input input in
    orbit_tracker.Orbit_tracker.outer_things
    |> String.Map.data
    |> List.fold ~init:0 ~f:(fun accum next -> accum + List.length next)
  ;;

  let%expect_test "find_all_outers" =
    let input =
      [ "COM)B"
      ; "B)C"
      ; "C)D"
      ; "D)E"
      ; "E)F"
      ; "B)G"
      ; "G)H"
      ; "D)I"
      ; "E)J"
      ; "J)K"
      ; "K)L"
      ]
    in
    Core.printf "%d\n%!" (find_all_outers input);
    [%expect {| 42 |}];
    Core.printf "%d\n%!" (find_all_outers (List.permute input));
    [%expect {| 42 |}]
  ;;
    
  let command =
    Command.basic
      ~summary:"Part 1 command"
      (Command.Param.map filename_param ~f:(fun filename ->
           (fun () ->
             let input = Stdio.In_channel.read_lines filename in
             Core.printf "There are %d total orbits.\n" (find_all_outers input))))
  ;;
end

module Part2 = struct
  let shortest_path_to_santa input =
    let orbit_tracker = Common.process_input input in
    let inner_than_you =
      String.Map.find_exn orbit_tracker.Orbit_tracker.inner_things "YOU"
    in
    let inner_than_santa =
      String.Map.find_exn orbit_tracker.Orbit_tracker.inner_things "SAN"
      |> String.Map.of_alist_exn
    in
    List.filter_map inner_than_you ~f:(fun (p, dist) ->
        match String.Map.find inner_than_santa p with
        | None -> None
        | Some dist' -> Some (dist' + dist))
    |> List.sort ~compare:Int.compare
    |> List.hd_exn
    |> fun raw_dist -> raw_dist - 2
  ;;

  let%expect_test "shortest_path_to_santa" =
    let input =
      [ "COM)B"
      ; "B)C"
      ; "C)D"
      ; "D)E"
      ; "E)F"
      ; "B)G"
      ; "G)H"
      ; "D)I"
      ; "E)J"
      ; "J)K"
      ; "K)L"
      ; "K)YOU"
      ; "I)SAN"
      ]
    in
    Core.printf "%d\n%!"(shortest_path_to_santa input);
    [%expect {| 4 |}]
  ;;
  
  let command =
    Command.basic
      ~summary:"Part 3 command"
      (Command.Param.map filename_param ~f:(fun filename ->
           (fun () ->
             let input = Stdio.In_channel.read_lines filename in
             Core.printf "The shortest path to santa requires %d jumps.\n%!" (shortest_path_to_santa input)
      )))
  ;;
end
             
let command =
  Command.group
    ~summary:"Advent of Code 2019 - Day 6"
    [ "1", Part1.command
    ; "2", Part2.command
    ]
;;
