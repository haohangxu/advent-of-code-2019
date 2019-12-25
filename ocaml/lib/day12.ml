open! Core
open! Std_internal

module Three_vector = struct
  type t =
    { x : int
    ; y : int
    ; z : int
    } [@@deriving sexp, fields, compare, hash]

  let zero =
    { x = 0
    ; y = 0
    ; z = 0
    }

  let add t1 t2 =
    { x = t1.x + t2.x
    ; y = t1.y + t2.y
    ; z = t1.z + t2.z
    }
end

module Moon = struct
  type t =
    { position : Three_vector.t
    ; velocity : Three_vector.t
    } [@@deriving sexp, compare, hash]

  let init position =
    { position; velocity = Three_vector.zero }
  ;;

  let energy t =
    let for_one (v : Three_vector.t) = abs v.x + abs v.y + abs v.z in
    let potential_energy = for_one t.position in
    let kinetic_energy = for_one t.velocity in
    potential_energy * kinetic_energy
  ;;
end
            
module Simulation = struct
  module State = struct
    module T = struct
      type t = Moon.t list [@@deriving sexp, compare, hash]
    end
    include T
    include Hashable.Make(T)
  end
        
  let get_new_velocity (t : State.t) moon = 
    List.fold t ~init:moon.Moon.velocity ~f:(fun velocity other_moon ->
        let get_delta f =
          if f moon.position < f other_moon.Moon.position
          then 1
          else if f moon.position > f other_moon.position
          then -1
          else 0
        in
        let delta = 
          { Three_vector.x = get_delta Three_vector.x
          ; y = get_delta Three_vector.y
          ; z = get_delta Three_vector.z
          }
        in
        Three_vector.add velocity delta)
  ;;

  let run_one t =
    List.map t ~f:(fun moon ->
        let velocity = get_new_velocity t moon in
        { Moon.position = Three_vector.add moon.position velocity; velocity })
  ;;

  let run_n t n = Fn.apply_n_times ~n run_one t

  let energy t =
    List.fold t ~init:0 ~f:(fun energy moon -> energy + (Moon.energy moon))
  ;;
end
                  
module Part1 = struct
  let command =
    let open Command.Param in
    Command.basic
      ~summary:"Part 1 command"
      (Command.Param.map2
         filename_param
         (flag "steps" (required int) ~doc:"INT number of steps to run simulation")
         ~f:(fun filename steps ->
           (fun () ->
             let moons =
               Stdio.In_channel.read_lines filename
               |> List.map ~f:(fun s -> Sexp.of_string s |> Three_vector.t_of_sexp)
               |> List.map ~f:Moon.init
             in
             let result = Simulation.run_n moons steps in
             Core.printf "Resulting energy: %d\n%!" (Simulation.energy result))))
  ;;
end
             
module Part2 = struct
  let run moons =
    let seen_situations = Simulation.State.Hash_set.create () in
    let rec run_until_repeated steps moons =
      if Hash_set.mem seen_situations moons
      then steps
      else
        (Hash_set.add seen_situations moons;
         let new_moons = Simulation.run_one moons in
         run_until_repeated (steps + 1) new_moons)
    in
    run_until_repeated 0 moons
  ;;
                         
  let command =
    Command.basic
      ~summary:"Part 2 command"
      (Command.Param.map filename_param
         ~f:(fun filename ->
           (fun () ->
             let moons =
               Stdio.In_channel.read_lines filename
               |> List.map ~f:(fun s -> Sexp.of_string s |> Three_vector.t_of_sexp)
               |> List.map ~f:Moon.init
             in
             Core.printf "It took %d steps to stabilize.\n%!" (run moons))))
  ;;
end
             
let command =
  Command.group
    ~summary:"Advent of Code 2019 - Day 12"
    [ "1", Part1.command
    ; "2", Part2.command
    ]
;;
