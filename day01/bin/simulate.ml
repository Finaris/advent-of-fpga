open! Core

(* Day 1: Secret Entrance *)
let () =
  let input_file =
    if Array.length (Sys.get_argv ()) > 1 then
      (Sys.get_argv ()).(1)
    else
      "day01/data/input.txt"
  in

  let input = In_channel.read_all input_file in
  let instructions = Day01.parse_instructions input in

  printf "Parsed %d instructions\n" (List.length instructions);

  (* Part 1 *)
  printf "\n=== Part 1: Count zeros at END of rotations ===\n";
  printf "Verifying with example input...\n";
  let example_result_1 = Day01.simulate Day01.example_instructions in
  printf "Example result: %d (expected: 3)\n" example_result_1;
  if example_result_1 <> 3 then
    printf "WARNING: Example result does not match expected value!\n";

  printf "Running with puzzle input...\n";
  let result_1 = Day01.simulate instructions in
  printf "Part 1 Answer: %d\n" result_1;

  (* Part 2 *)
  printf "\n=== Part 2: Count ALL zero crossings ===\n";
  printf "Verifying with example input...\n";
  let example_result_2 = Day01.simulate_part2 Day01.example_instructions in
  printf "Example result: %d (expected: 6)\n" example_result_2;
  if example_result_2 <> 6 then
    printf "WARNING: Example result does not match expected value!\n";

  printf "Running with puzzle input...\n";
  let result_2 = Day01.simulate_part2 instructions in
  printf "Part 2 Answer: %d\n" result_2
