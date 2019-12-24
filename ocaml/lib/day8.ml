open! Core
open! Std_internal

module Common = struct
  let get_layers input width height = 
    String.to_list input
    |> List.chunks_of ~length:(height * width)
  ;;

  let get_columns input width height = 
    let layers = get_layers input width height in
    List.init (height * width) ~f:(fun i ->
        List.map layers ~f:(fun l -> List.nth_exn l i))
  ;;

  let%expect_test "get_columns" =
    let input = "0222112222120000" in
    List.iter (get_columns input 2 2) ~f:(fun col ->
        Core.print_endline (String.of_char_list col));
    [%expect {|
      0120
      2120
      2210
      2220 |}]
  ;;

  let get_first_visible_pixel col =
    List.find_exn col ~f:(fun c -> not (Char.(=) c '2'))
  ;;

  let get_visible input width height =
    let columns = get_columns input width height in
    List.map columns ~f:get_first_visible_pixel
  ;;

  let%expect_test "get_visible" =
    let input = "0222112222120000" in
    Core.print_endline (get_visible input 2 2 |> String.of_char_list);
    [%expect {| 0110 |}]
  ;;

  let render input width height =
    get_visible input width height
    |> List.chunks_of ~length:width
    |> List.iter ~f:(fun row ->
           List.map row ~f:(function
               | '0' -> '@'
               | _ -> ' ')
           |> String.of_char_list
           |> Core.print_endline)
  ;;

  let%expect_test "render" =
    let input = "0222112222120000" in
    render input 2 2;
    [%expect {|
      @
       @ |}]
    ;;
end
          
module Part1 = struct
  let count char = List.count ~f:(Char.equal char)
                 
  let command =
    let open Command.Param in 
    Command.basic
      ~summary:"Part 1 command"
      (Command.Param.map3
         filename_param
         (flag "height" (required int) ~doc:"INT height")
         (flag "width" (required int) ~doc:"INT width")
         ~f:(fun filename height width ->
           (fun () ->
             let input = Stdio.In_channel.read_lines filename in
             assert (List.length input = 1);
             let layers = Common.get_layers (List.hd_exn input) width height in
             let layer_with_fewest_0s =
               List.sort layers ~compare:(Comparable.lift Int.compare ~f:(count '0'))
               |> List.hd_exn
             in
             let num_1s = count '1' layer_with_fewest_0s in
             let num_2s = count '2' layer_with_fewest_0s in
             Core.printf "The product of num 1s and num 2s in layer with fewest 0s is %d.\n%!" (num_1s * num_2s))))
  ;;
end
             
module Part2 = struct
  let command =
    let open Command.Param in 
    Command.basic
      ~summary:"Part 2 command"
      (Command.Param.map3
         filename_param
         (flag "height" (required int) ~doc:"INT height")
         (flag "width" (required int) ~doc:"INT width")
         ~f:(fun filename height width ->
           (fun () ->
             let input = Stdio.In_channel.read_lines filename in
             assert (List.length input = 1);
             Common.render (List.hd_exn input) width height)))
  ;;
end
             
let command =
  Command.group
    ~summary:"Advent of Code 2019 - Day 8"
    [ "1", Part1.command
    ; "2", Part2.command
    ]
;;
