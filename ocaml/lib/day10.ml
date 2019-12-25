open! Core
open! Std_internal
  
module Sign = struct
  type t =
    | Pos
    | Neg
    | Zero

  let classify i =
    match i with
    | 0 -> Zero
    | a when a > 0 -> Pos
    | a when a < 0 -> Neg
    | _ -> assert false
end
            
module Quadrant = struct
  type t =
    | I
    | II
    | III
    | IV
    [@@deriving hash, compare, sexp]
  ;;

  let classify (row, col) =
    match (Sign.classify row), (Sign.classify col) with
    | Pos, (Pos | Zero) -> I
    | (Neg | Zero), Pos -> II
    | Neg, (Neg | Zero) -> III
    | (Pos | Zero), Neg -> IV
    | Zero, Zero -> assert false
  ;;

  let%expect_test "classify" =
    let test row col = Core.printf !"%{sexp: t}\n%!" (classify (row, col)) in
    test 1 0;
    [%expect {| I |}];
    test 0 1;
    [%expect {| II |}];
    test (-1) 0;
    [%expect {| III |}];
    test 0 (-1);
    [%expect {| IV |}];
    test 1 1;
    [%expect {| I |}];
    test (-1) 1;
    [%expect {| II |}];
    test (-1) (-1);
    [%expect {| III |}];
    test 1 (-1);
    [%expect {| IV |}]
  ;;
end

module Slope = struct
  module T = struct
    type t =
      { quadrant : Quadrant.t
      ; numerator : int
      ; denominator : int
      } [@@deriving hash, sexp, compare]

    let resolve { quadrant; numerator; denominator } =
      let slope = (Float.of_int numerator) /. (Float.of_int denominator) in
      match quadrant with
      | I | III -> slope
      | II | IV -> -. slope
    ;;

    (* CR-someday hxu: This is also confusing. It's because we want the smallest
       to be positive Y and the largest to be positive Y - episolon. *)
    let slope_compare a b =
      match Quadrant.compare b.quadrant a.quadrant with
      | 0 ->
         (match a.denominator, b.denominator with
         | 0, 0 -> assert false
         | 0, _ -> 1
         | _, 0 -> -1
         | _, _ -> Float.compare (resolve a) (resolve b))
      | result -> result
    ;;
  end
  include T
  include Hashable.Make(T)
                  
  let create ~top ~bottom =
    let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
    let quadrant = Quadrant.classify (top, bottom) in
    let top, bottom = 
      if bottom = 0
      then 0, 0
      else (
        let gcd = gcd top bottom in
        top / gcd, bottom / gcd)
    in
    { quadrant
    ; numerator = abs top
    ; denominator = abs bottom
    }
  ;;

  (* CR-someday hxu: This is way confusing. It has to do with the fact the
     quadrant system is based on the origin being in the lower left whereas the
     asteroid coordinate system is based on the origin being in the top left. *)
  let compute ~from ~to_ = 
    create
      ~top:(from.Coord.row - to_.Coord.row)
      ~bottom:(to_.col - from.col)
  ;;
  
  let%expect_test "slope_compare" = 
    let test a b = Core.printf "%d\n%!" (slope_compare a b) in
    test (create ~top:1 ~bottom:1) (create ~top:1 ~bottom:2);
    [%expect {| 1 |}];
    test (create ~top:(-1) ~bottom:1) (create ~top:(-1) ~bottom:2);
    [%expect {| -1 |}];
    test (create ~top:(-1) ~bottom:(-1)) (create ~top:(-1) ~bottom:(-2));
    [%expect {| 1 |}];
    test (create ~top:1 ~bottom:(-1)) (create ~top:1 ~bottom:(-2));
    [%expect {| -1 |}];
    test (create ~top:1 ~bottom:1) (create ~top:1 ~bottom:(-1));
    [%expect {| 1 |}];
    test (create ~top:(-1) ~bottom:1) (create ~top:1 ~bottom:(-1));
    [%expect {| 1 |}]
 ;;
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

  let test3 =
    {|
.#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.....#...###..
..#.#.....#....##
|}
    |> parse_for_tests
  ;;
  
  let find_asteroids field =
    List.foldi ~init:[] field ~f:(fun row accum row_elts ->
        List.foldi ~init:accum row_elts ~f:(fun col accum elt ->
            if Char.(=) elt '#'
            then Coord.{ row; col }  :: accum
            else accum))
  ;;

  let%expect_test "find_asteroids" =
    let field = test1 in
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
        else Some (Slope.compute ~from:asteroid ~to_:a, a))
    |> Slope.Table.of_alist_multi
    |> Slope.Table.map
         ~f:(List.sort ~compare:(fun a b ->
                 let dist_a = Coord.dist a asteroid in
                 let dist_b = Coord.dist b asteroid in
                 Int.compare dist_a dist_b))
  ;;
  
  let%expect_test "get_slopes" =
    let asteroids = find_asteroids test1 in
    Core.printf !"%{sexp:Coord.t list Slope.Table.t}\n%!" (get_slopes { Coord.row = 4; col = 3 } asteroids);
    [%expect {|
      ((((quadrant I) (numerator 0) (denominator 0)) (((row 2) (col 3))))
       (((quadrant I) (numerator 1) (denominator 1)) (((row 3) (col 4))))
       (((quadrant I) (numerator 2) (denominator 1)) (((row 2) (col 4))))
       (((quadrant I) (numerator 4) (denominator 1)) (((row 0) (col 4))))
       (((quadrant II) (numerator 0) (denominator 1)) (((row 4) (col 4))))
       (((quadrant IV) (numerator 1) (denominator 1)) (((row 2) (col 1))))
       (((quadrant IV) (numerator 2) (denominator 1))
        (((row 2) (col 2)) ((row 0) (col 1))))
       (((quadrant IV) (numerator 2) (denominator 3)) (((row 2) (col 0))))) |}]
  ;;
end
        
module Part1 = struct
  let count_visible asteroid asteroids =
    Common.get_slopes asteroid asteroids
    |> Slope.Table.keys
    |> List.length
  ;;

  let%expect_test "count_visible" =
    let asteroids = Common.find_asteroids Common.test1 in
    Core.printf "%d\n%!" (count_visible { Coord.row = 4; col = 3 } asteroids);
    [%expect {| 8 |}];
    Core.printf "%d\n%!" (count_visible { Coord.row = 4; col = 4 } asteroids);
    [%expect {| 7 |}];
  ;;

  let find_best' asteroids =
    List.map asteroids ~f:(fun a -> a, count_visible a asteroids)
    |> List.sort ~compare:(Comparable.lift Int.compare ~f:snd)
    |> List.last_exn
  ;;

  let find_best field =
    let asteroids = Common.find_asteroids field in
    find_best' asteroids |> snd
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
  let get_sorted_slopes' asteroid asteroids = 
    Common.get_slopes asteroid asteroids
    |> Slope.Table.to_alist
    |> List.sort ~compare:(fun (slope1, _) (slope2, _) ->
           (* We want to sort in reverse. *)
           Slope.slope_compare slope2 slope1)
  ;;

  let get_sorted_slopes asteroid asteroids =
    get_sorted_slopes' asteroid asteroids
    |> List.map ~f:snd
  ;;
  
  let%expect_test "get_sorted_slopes'" =
    let asteroids = Common.test3 |> Common.find_asteroids in
    Core.printf !"%{sexp: (Slope.t * Coord.t list) list}\n%!" (get_sorted_slopes' { Coord.row = 3; col = 8 } asteroids);
    [%expect {|
      ((((quadrant I) (numerator 0) (denominator 0))
        (((row 1) (col 8)) ((row 0) (col 8))))
       (((quadrant I) (numerator 3) (denominator 1)) (((row 0) (col 9))))
       (((quadrant I) (numerator 2) (denominator 1)) (((row 1) (col 9))))
       (((quadrant I) (numerator 3) (denominator 2)) (((row 0) (col 10))))
       (((quadrant I) (numerator 1) (denominator 1))
        (((row 2) (col 9)) ((row 1) (col 10))))
       (((quadrant I) (numerator 2) (denominator 3)) (((row 1) (col 11))))
       (((quadrant I) (numerator 1) (denominator 2))
        (((row 1) (col 12)) ((row 0) (col 14))))
       (((quadrant I) (numerator 1) (denominator 3)) (((row 2) (col 11))))
       (((quadrant I) (numerator 2) (denominator 7)) (((row 1) (col 15))))
       (((quadrant I) (numerator 1) (denominator 4))
        (((row 2) (col 12)) ((row 1) (col 16))))
       (((quadrant I) (numerator 1) (denominator 5)) (((row 2) (col 13))))
       (((quadrant I) (numerator 1) (denominator 6)) (((row 2) (col 14))))
       (((quadrant I) (numerator 1) (denominator 7)) (((row 2) (col 15))))
       (((quadrant II) (numerator 0) (denominator 1))
        (((row 3) (col 12)) ((row 3) (col 13)) ((row 3) (col 14))))
       (((quadrant II) (numerator 1) (denominator 8)) (((row 4) (col 16))))
       (((quadrant II) (numerator 1) (denominator 7)) (((row 4) (col 15))))
       (((quadrant II) (numerator 1) (denominator 2)) (((row 4) (col 10))))
       (((quadrant III) (numerator 1) (denominator 4)) (((row 4) (col 4))))
       (((quadrant III) (numerator 1) (denominator 6)) (((row 4) (col 2))))
       (((quadrant IV) (numerator 0) (denominator 1)) (((row 3) (col 2))))
       (((quadrant IV) (numerator 1) (denominator 8)) (((row 2) (col 0))))
       (((quadrant IV) (numerator 1) (denominator 7)) (((row 2) (col 1))))
       (((quadrant IV) (numerator 1) (denominator 4)) (((row 1) (col 0))))
       (((quadrant IV) (numerator 2) (denominator 7)) (((row 1) (col 1))))
       (((quadrant IV) (numerator 1) (denominator 3)) (((row 2) (col 5))))
       (((quadrant IV) (numerator 3) (denominator 7)) (((row 0) (col 1))))
       (((quadrant IV) (numerator 2) (denominator 3)) (((row 1) (col 5))))
       (((quadrant IV) (numerator 1) (denominator 1)) (((row 1) (col 6))))
       (((quadrant IV) (numerator 3) (denominator 2)) (((row 0) (col 6))))
       (((quadrant IV) (numerator 3) (denominator 1)) (((row 0) (col 7))))) |}]
  ;;
    
  let find_nth_vaporized n asteroid asteroids =
    let asteroids = get_sorted_slopes asteroid asteroids in
    let rec vaporize_n n asteroids =
      if n = 1
      then List.hd_exn (List.hd_exn asteroids)
      else
        let hd = List.hd_exn asteroids in
        let tl = List.tl_exn asteroids in
        vaporize_n (n - 1) (tl @ [ (List.tl_exn hd) ])
    in
    vaporize_n n asteroids
  ;;

  let%expect_test "find_nth_vaporized" =
    let asteroids = Common.test3 |> Common.find_asteroids in
    Core.printf !"%{sexp: Coord.t}\n%!" (find_nth_vaporized 1 { Coord.row = 3; col = 8 } asteroids);
    [%expect {| ((row 1) (col 8)) |}];
    Core.printf !"%{sexp: Coord.t}\n%!" (find_nth_vaporized 9 { Coord.row = 3; col = 8 } asteroids);
    [%expect {| ((row 1) (col 15)) |}]
  ;;
  
  let command =
    let open Command.Param in
    Command.basic
      ~summary:"Part 2 command"
      (Command.Param.map2
         filename_param
         (flag "n" (required int) ~doc:"INT nth asteroid")
         ~f:(fun filename n ->
           (fun () ->
             let input = Stdio.In_channel.read_lines filename |> List.map ~f:String.to_list in
             let asteroids = Common.find_asteroids input in
             let best = Part1.find_best' asteroids |> fst in
             Core.printf !"%{sexp: Coord.t}\n%!" (find_nth_vaporized n best asteroids))))
  ;;
end
             
let command =
  Command.group
    ~summary:"Advent of Code 2019 - Day 10"
    [ "1", Part1.command
    ; "2", Part2.command
    ]
;;
