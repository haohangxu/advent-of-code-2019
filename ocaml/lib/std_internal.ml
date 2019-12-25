open! Core

let filename_param =
  let open Command.Param in
  anon ("filename" %: string)
;;

module Coord = struct
  module T = struct
    type t =
      { row : int
      ; col : int
      } [@@deriving hash, sexp, compare]
  end
  include T
  include Hashable.Make(T)
  include Comparable.Make(T)
        
  let dist a b =
    (abs (a.row - b.row)) + (abs (a.col - b.col))
  ;;
end
