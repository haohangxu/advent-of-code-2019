open! Core
open! Std_internal

module Program_state : sig
  module Memory : sig
    type t = int Array.t

    val of_string : string -> t
  end
       
  module Status : sig
    type t =
      | Halted
      | Blocked_on_input
    [@@deriving sexp_of]
  end

  type t =
    { memory : Memory.t
    ; instr_num : int
    ; inputs : int list
    ; outputs : int list
    ; status : Status.t
    ; relative_base : int 
    } [@@deriving sexp_of]

  val init : int Array.t -> initial_inputs : int list -> t
end
  
val interpret : Program_state.t -> Program_state.t
