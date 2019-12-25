open! Core
open! Std_internal

module Common = struct
  let to_digits i =
    Int.to_string i
    |> String.to_list
    |> List.map ~f:(fun c -> String.of_char c |> Int.of_string)
  ;;

  let%expect_test "to_digits" =
    Core.printf !"%{sexp: int list}\n%!" (to_digits 123);
    [%expect {| (1 2 3) |}];
    Core.printf !"%{sexp: int list}\n%!" (to_digits 100);
    [%expect {| (1 0 0) |}]
  ;;

  let digits_never_decrease digits =
    [%compare.equal: int list]
      (List.sort digits ~compare:Int.compare)
      digits
  ;;

  let contains_two_repeats digits =
    List.existsi digits ~f:(fun i d ->
        match List.nth digits (i + 1) with
        | None -> false
        | Some next_digit -> next_digit = d)
  ;;

  let contains_two_repeats_but_not_three digits =
    List.existsi digits ~f:(fun i d ->
        match List.nth digits (i - 1) with
        | Some prev_digit when prev_digit = d -> false
        | Some _ | None -> 
           (match List.nth digits (i + 1) with
           | None -> false
           | Some next_digit ->
              if not (next_digit = d)
              then false
              else
                (match List.nth digits (i + 2) with
                 | None -> true
                 | Some next_next_digit -> not (next_next_digit = d))))
  ;;

  let%expect_test "contains_two_repeats_but_not_three" =
    let test i = 
      Core.printf "%b\n%!" (contains_two_repeats_but_not_three (to_digits i))
    in
    test 112233;
    [%expect {| true |}];
    test 111122;
    [%expect {| true |}];
    test 123444;
    [%expect {| false |}]
  ;;

  let number_possibilities low high ~f =
    let range = List.range low high in
    List.filter range ~f
    |> List.length
  ;;
end

module Part1 = struct
  let fits_criteria i =
    let digits = Common.to_digits i in
    Common.digits_never_decrease digits && Common.contains_two_repeats digits
  ;;

  let%expect_test "fits_criteria" =
    Core.printf "%b\n%!" (fits_criteria 111111);
    [%expect {| true |}];
    Core.printf "%b\n%!" (fits_criteria 223450);
    [%expect {| false |}];
    Core.printf "%b\n%!" (fits_criteria 123789);
    [%expect {| false |}]
  ;;
  
  let command =
    let open Command.Param in
    Command.basic
      ~summary:"Part 1 command"
      (Command.Param.map2
         (flag "low" (required int) ~doc:"INT range low")
         (flag "high" (required int) ~doc:"INT range high")
         ~f:(fun low high ->
           (fun () ->
             let poss = Common.number_possibilities low high ~f:fits_criteria in
             Core.printf "There are %d possible passwords.\n%!" poss
      )))
  ;;
end

module Part2 = struct
  let fits_criteria i =
    let digits = Common.to_digits i in
    Common.digits_never_decrease digits && Common.contains_two_repeats_but_not_three digits
  ;;

  let%expect_test "fits_criteria" =
    Core.printf "%b\n%!" (fits_criteria 111111);
    [%expect {| false |}];
    Core.printf "%b\n%!" (fits_criteria 223450);
    [%expect {| false |}];
    Core.printf "%b\n%!" (fits_criteria 123789);
    [%expect {| false |}];
    Core.printf "%b\n%!" (fits_criteria 112233);
    [%expect {| true |}];
    Core.printf "%b\n%!" (fits_criteria 123444);
    [%expect {| false |}];
    Core.printf "%b\n%!" (fits_criteria 111122);
    [%expect {| true |}]
  ;;
  
  let command =
    let open Command.Param in
    Command.basic
      ~summary:"Part 2 command"
      (Command.Param.map2
         (flag "low" (required int) ~doc:"INT range low")
         (flag "high" (required int) ~doc:"INT range high")
         ~f:(fun low high ->
           (fun () ->
             let poss = Common.number_possibilities low high ~f:fits_criteria in
             Core.printf "There are %d possible passwords.\n%!" poss
      )))
  ;;
end
             
let command =
  Command.group
    ~summary:"Advent of Code 2019 - Day 4"
    [ "1", Part1.command
    ; "2", Part2.command
    ]
;;
