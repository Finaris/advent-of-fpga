(** Day 1: Secret Entrance - Dial safe combination counter *)

(** Input interface for the circuit *)
module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    }
  [@@deriving hardcaml]
end

(** Output interface for the circuit *)
module O : sig
  type 'a t =
    { done_ : 'a
    ; count : 'a
    ; position : 'a
    }
  [@@deriving hardcaml]
end

(** Parse a single instruction line like "L68" or "R10"
    Returns (direction, distance) where direction=true means Right *)
val parse_instruction : string -> (bool * int) option

(** Parse all instructions from input string *)
val parse_instructions : string -> (bool * int) list

(** Example instructions from the problem *)
val example_instructions : (bool * int) list

(** {1 Part 1: Count zeros at end of rotations} *)

module Part1 : sig
  val simulate : (bool * int) list -> int
end

(** {1 Part 2: Count all zero crossings} *)

module Part2 : sig
  val simulate : (bool * int) list -> int
end

(** {1 Backward-compatible API} *)

(** Simulate Part 1 and return the count of zeros *)
val simulate : (bool * int) list -> int

(** Simulate Part 2 and return the count of all zero crossings *)
val simulate_part2 : (bool * int) list -> int
