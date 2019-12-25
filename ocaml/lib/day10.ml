open! Core
open! Std_internal

module Coord = struct
  type t =
    { row : int
    ; col : int
    } [@@deriving sexp_of, compare]

  let%expect_test "compare" =
    let compare (x, y) = Core.printf !"%{sexp:t} %{sexp:t} - %d\n%!" x y (compare x y) in
    let all =
      List.map (List.cartesian_product [0; 1] [0; 1])~f:(fun (row, col) -> { row; col })
    in
    List.iter (List.cartesian_product all all) ~f:compare;
    [%expect {|
      ((row 0) (col 0)) ((row 0) (col 0)) - 0
      ((row 0) (col 0)) ((row 0) (col 1)) - -1
      ((row 0) (col 0)) ((row 1) (col 0)) - -1
      ((row 0) (col 0)) ((row 1) (col 1)) - -1
      ((row 0) (col 1)) ((row 0) (col 0)) - 1
      ((row 0) (col 1)) ((row 0) (col 1)) - 0
      ((row 0) (col 1)) ((row 1) (col 0)) - -1
      ((row 0) (col 1)) ((row 1) (col 1)) - -1
      ((row 1) (col 0)) ((row 0) (col 0)) - 1
      ((row 1) (col 0)) ((row 0) (col 1)) - 1
      ((row 1) (col 0)) ((row 1) (col 0)) - 0
      ((row 1) (col 0)) ((row 1) (col 1)) - -1
      ((row 1) (col 1)) ((row 0) (col 0)) - 1
      ((row 1) (col 1)) ((row 0) (col 1)) - 1
      ((row 1) (col 1)) ((row 1) (col 0)) - 1
      ((row 1) (col 1)) ((row 1) (col 1)) - 0 |}]
  ;;
end
             
module Slope = struct
  module T = struct
    type t =
      { is_positive : bool
      ; is_greater : bool
      ; numerator : int
      ; denominator : int
      } [@@deriving compare, hash, sexp]
  end
  include T
  include Hashable.Make(T)
               
  let create ~top ~bottom ~is_greater =
    let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
    if bottom = 0
    then { is_positive = true; is_greater; numerator = 0; denominator = 0 }
    else 
      let is_positive =
        (top > 0 && bottom > 0) || (top < 0 && bottom < 0)
      in
      let top, bottom = abs top, abs bottom in
      let gcd = gcd top bottom in
      { is_positive
      ; is_greater
      ; numerator = top / gcd
      ; denominator = bottom / gcd
      }
  ;;

  let compute coord1 coord2 =
    create
      ~is_greater:(Coord.compare coord1 coord2 > 0)
      ~top:(coord1.Coord.row - coord2.Coord.row)
      ~bottom:(coord1.col - coord2.col)
  ;;

  (* pos y is smallest, pos y rotated epsilon counterclockwise is largest *)
  (* let compare slope1 slope2 =
   *   let  =  *)
    
end
             
module Common = struct
  let parse_for_tests str =
    String.split str ~on:'\n'
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> List.map ~f:String.to_list
  ;;

  let test1 =
    {|
.#..#
.....
#####
....#
...##
|}
    |> parse_for_tests
  ;;

  let test2 =
    {|
......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####
|}
    |> parse_for_tests
  ;;
end
        
module Part1 = struct
  let find_asteroids field =
    List.foldi ~init:[] field ~f:(fun row accum row_elts ->
        List.foldi ~init:accum row_elts ~f:(fun col accum elt ->
            if Char.(=) elt '#'
            then Coord.{ row; col }  :: accum
            else accum))
  ;;

  let%expect_test "find_asteroids" =
    let field = Common.test1 in
    Core.printf !"%{sexp: Coord.t list}\n%!" (find_asteroids field);
    [%expect {|
      (((row 4) (col 4)) ((row 4) (col 3)) ((row 3) (col 4)) ((row 2) (col 4))
       ((row 2) (col 3)) ((row 2) (col 2)) ((row 2) (col 1)) ((row 2) (col 0))
       ((row 0) (col 4)) ((row 0) (col 1))) |}]
  ;;

  let get_slopes asteroid asteroids =
    List.filter_map asteroids ~f:(fun (a : Coord.t) ->
        if [%compare.equal: Coord.t] a asteroid
        then None
        else Some (Slope.compute a asteroid, a))
    |> Slope.Table.of_alist_multi
  ;;

  let count_visible asteroid asteroids =
    get_slopes asteroid asteroids
    |> Slope.Table.keys
    |> List.length
  ;;

  let%expect_test "count_visible" =
    let asteroids = find_asteroids Common.test1 in
    Core.printf "%d\n%!" (count_visible { Coord.row = 4; col = 3 } asteroids);
    [%expect {| 8 |}];
    Core.printf "%d\n%!" (count_visible { Coord.row = 4; col = 4 } asteroids);
    [%expect {| 7 |}];
  ;;

  let find_best field =
    let asteroids = find_asteroids field in
    List.map asteroids ~f:(fun a -> count_visible a asteroids)
    |> List.sort ~compare:Int.compare
    |> List.last_exn
  ;;

  let%expect_test "find_best" =
    Core.printf !"%d\n%!" (find_best Common.test1);
    [%expect {| 8 |}];
    Core.printf !"%d\n%!" (find_best Common.test2);
    [%expect {| 33 |}]
  ;;
    
  let command =
    Command.basic
      ~summary:"Part 1 command"
      (Command.Param.map filename_param ~f:(fun filename ->
           (fun () ->
             let input = Stdio.In_channel.read_lines filename |> List.map ~f:String.to_list in
             Core.printf "At best, we can monitor %d asteroids.\n%!" (find_best input);
             ())))
  ;;
end
             
module Part2 = struct
  let command =
    Command.basic
      ~summary:"Part 2 command"
      (Command.Param.map filename_param ~f:(fun filename ->
           (fun () ->
             let input = Stdio.In_channel.read_lines filename |> List.map ~f:String.to_list in
             ignore (input);
             ())))
  ;;
end
             
let command =
  Command.group
    ~summary:"Advent of Code 2019 - Day 10"
    [ "1", Part1.command
    ; "2", Part2.command
    ]
;;
