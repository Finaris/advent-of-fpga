open! Core
open Hardcaml
open Signal

(* ============================================================================
   Common Constants and Parsing
   ============================================================================ *)

let direction_bit = 10
let distance_width = 10
let instruction_width = 11
let position_width = 7  (* 0-99 fits in 7 bits *)
let counter_width = 16  (* plenty for counting zeros *)

(* Parse a single instruction line like "L68" or "R10" *)
let parse_instruction line =
  let line = String.strip line in
  if String.is_empty line then None
  else
    let direction = Char.equal (String.get line 0) 'R' in
    let distance = Int.of_string (String.sub line ~pos:1 ~len:(String.length line - 1)) in
    Some (direction, distance)

(* Parse all instructions from a string *)
let parse_instructions input =
  String.split_lines input
  |> List.filter_map ~f:parse_instruction

(* Example instructions from problem *)
let example_instructions =
  [ (false, 68)   (* L68 *)
  ; (false, 30)   (* L30 *)
  ; (true, 48)    (* R48 *)
  ; (false, 5)    (* L5 *)
  ; (true, 60)    (* R60 *)
  ; (false, 55)   (* L55 *)
  ; (false, 1)    (* L1 *)
  ; (false, 99)   (* L99 *)
  ; (true, 14)    (* R14 *)
  ; (false, 82)   (* L82 *)
  ]

(* ============================================================================
   Part 1: Count zeros at END of rotations
   ============================================================================ *)

module Part1 = struct
  (* Encode instruction: bit 10 = direction, bits 9:0 = distance mod 100 *)
  let encode_instruction (direction, distance) =
    let dir_bit = if direction then 1 lsl direction_bit else 0 in
    let dist_mod = distance mod 100 in
    dir_bit lor dist_mod

  let make_rom_contents instructions =
    List.map instructions ~f:(fun instr ->
      of_constant (Constant.of_int ~width:instruction_width (encode_instruction instr)))

  (* Compute (a + b) mod 100 where a is 0-99 and b is 0-99 *)
  let mod100_add a b =
    let a8 = uresize a ~width:8 in
    let b8 = uresize b ~width:8 in
    let sum = a8 +: b8 in
    let sum_minus_100 = sum -:. 100 in
    let result = mux2 (sum >=:. 100) sum_minus_100 sum in
    sel_bottom result ~width:position_width

  (* Compute (a - b + 100) mod 100 where a is 0-99 and b is 0-99 *)
  let mod100_sub a b =
    let a8 = uresize a ~width:8 in
    let b8 = uresize b ~width:8 in
    let diff = a8 -: b8 in
    let diff_plus_100 = diff +:. 100 in
    let result = mux2 (a8 >=: b8) diff diff_plus_100 in
    sel_bottom result ~width:position_width

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { done_ : 'a
      ; count : 'a [@bits counter_width]
      ; position : 'a [@bits position_width]
      }
    [@@deriving hardcaml]
  end

  let create ~rom_contents (i : _ I.t) =
    let open I in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let num_instructions = List.length rom_contents in
    let addr_width = Int.ceil_log2 (num_instructions + 1) in

    let running = wire 1 in
    let running_reg = reg spec ~enable:vdd (mux2 i.start vdd (mux2 running running gnd)) in
    assign running running_reg;

    let addr = wire addr_width in
    let addr_next = mux2 i.start (zero addr_width) (addr +:. 1) in
    let addr_reg = reg spec ~enable:(i.start |: running) addr_next in
    assign addr addr_reg;

    let done_processing = addr ==:. num_instructions in

    let instruction = mux addr rom_contents in
    let direction = bit instruction ~pos:direction_bit in
    let distance = sel_bottom instruction ~width:distance_width in
    let distance_7 = sel_bottom distance ~width:position_width in

    let position = wire position_width in
    let new_position =
      mux2 direction
        (mod100_add position distance_7)
        (mod100_sub position distance_7)
    in
    let initial_pos = of_constant (Constant.of_int ~width:position_width 50) in
    let position_reg = reg spec ~enable:(i.start |: running)
      (mux2 i.start initial_pos new_position) in
    assign position position_reg;

    (* Only count when running AND not yet done processing *)
    let is_zero = new_position ==:. 0 in
    let should_count = running &: (~:done_processing) &: is_zero in
    let count = wire counter_width in
    let count_next = mux2 should_count (count +:. 1) count in
    let count_reg = reg spec ~enable:vdd (mux2 i.start (zero counter_width) count_next) in
    assign count count_reg;

    let done_wire = wire 1 in
    let done_state = reg spec ~enable:vdd
      (mux2 i.start gnd (mux2 (running &: done_processing) vdd done_wire)) in
    assign done_wire done_state;

    { O.done_ = done_wire; count; position }

  let simulate instructions =
    let rom_contents = make_rom_contents instructions in
    let module Sim = Cyclesim.With_interface (I) (O) in
    let sim = Sim.create (create ~rom_contents) in
    let inputs = Cyclesim.inputs sim in
    let outputs = Cyclesim.outputs sim in

    inputs.start := Bits.of_int_trunc ~width:1 0;
    Cyclesim.cycle sim;
    inputs.start := Bits.of_int_trunc ~width:1 1;
    Cyclesim.cycle sim;
    inputs.start := Bits.of_int_trunc ~width:1 0;

    let max_cycles = List.length instructions + 10 in
    let rec run_until_done cycles =
      if cycles > max_cycles then failwith "Simulation did not complete"
      else if Bits.to_int_trunc !(outputs.done_) = 1 then
        Bits.to_int_trunc !(outputs.count)
      else begin
        Cyclesim.cycle sim;
        run_until_done (cycles + 1)
      end
    in
    run_until_done 0
end

(* ============================================================================
   Part 2: Count ALL zero crossings (including during rotations)
   ============================================================================ *)

module Part2 = struct
  (* For Part 2, we need full distance (not mod 100) for zero crossing calculation *)
  let encode_instruction (direction, distance) =
    let dir_bit = if direction then 1 lsl direction_bit else 0 in
    dir_bit lor distance

  let make_rom_contents instructions =
    List.map instructions ~f:(fun instr ->
      of_constant (Constant.of_int ~width:instruction_width (encode_instruction instr)))

  (* Integer division by 100: x / 100 for x up to ~1100
     Returns quotient in 4 bits (max value ~11) *)
  let div100 x =
    let w = 4 in
    let x12 = uresize x ~width:12 in
    (* Cascade of comparisons for division *)
    mux2 (x12 >=:. 1100) (of_constant (Constant.of_int ~width:w 11))
      (mux2 (x12 >=:. 1000) (of_constant (Constant.of_int ~width:w 10))
        (mux2 (x12 >=:. 900) (of_constant (Constant.of_int ~width:w 9))
          (mux2 (x12 >=:. 800) (of_constant (Constant.of_int ~width:w 8))
            (mux2 (x12 >=:. 700) (of_constant (Constant.of_int ~width:w 7))
              (mux2 (x12 >=:. 600) (of_constant (Constant.of_int ~width:w 6))
                (mux2 (x12 >=:. 500) (of_constant (Constant.of_int ~width:w 5))
                  (mux2 (x12 >=:. 400) (of_constant (Constant.of_int ~width:w 4))
                    (mux2 (x12 >=:. 300) (of_constant (Constant.of_int ~width:w 3))
                      (mux2 (x12 >=:. 200) (of_constant (Constant.of_int ~width:w 2))
                        (mux2 (x12 >=:. 100) (of_constant (Constant.of_int ~width:w 1))
                          (of_constant (Constant.of_int ~width:w 0))))))))))))

  (* Compute zero crossings for a rotation
     Right from p by d: count = (p + d) / 100
     Left from p by d:
       - if p = 0: count = d / 100
       - if d >= p: count = (d - p) / 100 + 1
       - else: count = 0 *)
  let zero_crossings ~position ~direction ~distance =
    let p = uresize position ~width:12 in
    let d = uresize distance ~width:12 in
    let one = of_constant (Constant.of_int ~width:4 1) in

    (* Right: (p + d) / 100 *)
    let right_count = div100 (p +: d) in

    (* Left: complex logic *)
    let p_is_zero = p ==:. 0 in
    let d_ge_p = d >=: p in
    let left_count =
      mux2 p_is_zero
        (div100 d)                              (* p = 0: d / 100 *)
        (mux2 d_ge_p
          (div100 (d -: p) +: one)              (* d >= p: (d-p)/100 + 1 *)
          (of_constant (Constant.of_int ~width:4 0)))  (* d < p: 0 *)
    in
    mux2 direction right_count left_count

  (* Compute new position (same as Part 1 but with full distance) *)
  let mod100_add a b =
    let a8 = uresize a ~width:8 in
    let b8 = uresize b ~width:8 in
    let sum = a8 +: b8 in
    let sum_minus_100 = sum -:. 100 in
    mux2 (sum >=:. 100) sum_minus_100 sum

  let mod100_sub a b =
    let a8 = uresize a ~width:8 in
    let b8 = uresize b ~width:8 in
    let diff = a8 -: b8 in
    let diff_plus_100 = diff +:. 100 in
    mux2 (a8 >=: b8) diff diff_plus_100

  module I = Part1.I
  module O = Part1.O

  let create ~rom_contents (i : _ I.t) =
    let open I in
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let num_instructions = List.length rom_contents in
    let addr_width = Int.ceil_log2 (num_instructions + 1) in

    let running = wire 1 in
    let running_reg = reg spec ~enable:vdd (mux2 i.start vdd (mux2 running running gnd)) in
    assign running running_reg;

    let addr = wire addr_width in
    let addr_next = mux2 i.start (zero addr_width) (addr +:. 1) in
    let addr_reg = reg spec ~enable:(i.start |: running) addr_next in
    assign addr addr_reg;

    let done_processing = addr ==:. num_instructions in

    let instruction = mux addr rom_contents in
    let direction = bit instruction ~pos:direction_bit in
    let distance = sel_bottom instruction ~width:distance_width in

    (* Position calculation uses distance mod 100 *)
    let dist_mod_100 =
      let d = uresize distance ~width:12 in
      let sub100 = d -:. 100 in
      let sub200 = d -:. 200 in
      let sub300 = d -:. 300 in
      (* etc - for values up to ~1000, max mod is done with 10 subtractions *)
      let result =
        mux2 (d >=:. 1000) (d -:. 1000)
          (mux2 (d >=:. 900) (d -:. 900)
            (mux2 (d >=:. 800) (d -:. 800)
              (mux2 (d >=:. 700) (d -:. 700)
                (mux2 (d >=:. 600) (d -:. 600)
                  (mux2 (d >=:. 500) (d -:. 500)
                    (mux2 (d >=:. 400) (d -:. 400)
                      (mux2 (d >=:. 300) sub300
                        (mux2 (d >=:. 200) sub200
                          (mux2 (d >=:. 100) sub100 d)))))))))
      in
      sel_bottom result ~width:7
    in

    let position = wire position_width in
    let new_position =
      let pos8 = uresize position ~width:8 in
      let dist8 = uresize dist_mod_100 ~width:8 in
      let result = mux2 direction
        (mod100_add pos8 dist8)
        (mod100_sub pos8 dist8)
      in
      sel_bottom result ~width:position_width
    in
    let initial_pos = of_constant (Constant.of_int ~width:position_width 50) in
    let position_reg = reg spec ~enable:(i.start |: running)
      (mux2 i.start initial_pos new_position) in
    assign position position_reg;

    (* Zero crossing count for this rotation *)
    let crossings = zero_crossings ~position ~direction ~distance in

    (* Accumulate crossings - only when running AND not yet done processing *)
    let should_count = running &: (~:done_processing) in
    let count = wire counter_width in
    let crossings_wide = uresize crossings ~width:counter_width in
    let count_next = mux2 should_count (count +: crossings_wide) count in
    let count_reg = reg spec ~enable:vdd (mux2 i.start (zero counter_width) count_next) in
    assign count count_reg;

    let done_wire = wire 1 in
    let done_state = reg spec ~enable:vdd
      (mux2 i.start gnd (mux2 (running &: done_processing) vdd done_wire)) in
    assign done_wire done_state;

    { O.done_ = done_wire; count; position }

  let simulate instructions =
    let rom_contents = make_rom_contents instructions in
    let module Sim = Cyclesim.With_interface (I) (O) in
    let sim = Sim.create (create ~rom_contents) in
    let inputs = Cyclesim.inputs sim in
    let outputs = Cyclesim.outputs sim in

    inputs.start := Bits.of_int_trunc ~width:1 0;
    Cyclesim.cycle sim;
    inputs.start := Bits.of_int_trunc ~width:1 1;
    Cyclesim.cycle sim;
    inputs.start := Bits.of_int_trunc ~width:1 0;

    let max_cycles = List.length instructions + 10 in
    let rec run_until_done cycles =
      if cycles > max_cycles then failwith "Simulation did not complete"
      else if Bits.to_int_trunc !(outputs.done_) = 1 then
        Bits.to_int_trunc !(outputs.count)
      else begin
        Cyclesim.cycle sim;
        run_until_done (cycles + 1)
      end
    in
    run_until_done 0
end

(* ============================================================================
   Backward-compatible API
   ============================================================================ *)

module I = Part1.I
module O = Part1.O

let simulate = Part1.simulate
let simulate_part2 = Part2.simulate
