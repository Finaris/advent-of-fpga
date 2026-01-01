open! Core

let%expect_test "parse instruction" =
  let test s =
    match Day01.parse_instruction s with
    | None -> printf "None\n"
    | Some (dir, dist) ->
      printf "%s %d\n" (if dir then "R" else "L") dist
  in
  test "L68";
  test "R10";
  test "L5";
  test "R99";
  test "";
  [%expect {|
    L 68
    R 10
    L 5
    R 99
    None |}]

let%expect_test "parse instructions" =
  let input = "L68\nL30\nR48\nL5\nR60\n" in
  let instructions = Day01.parse_instructions input in
  List.iter instructions ~f:(fun (dir, dist) ->
    printf "%s%d\n" (if dir then "R" else "L") dist);
  [%expect {|
    L68
    L30
    R48
    L5
    R60 |}]

let%expect_test "part 1 - example simulation" =
  (* Example from problem:
     Starting at 50:
     L68 -> 82
     L30 -> 52
     R48 -> 0  (count: 1)
     L5  -> 95
     R60 -> 55
     L55 -> 0  (count: 2)
     L1  -> 99
     L99 -> 0  (count: 3)
     R14 -> 14
     L82 -> 32
     Answer: 3 *)
  let result = Day01.simulate Day01.example_instructions in
  printf "Part 1 - Zero count: %d\n" result;
  [%expect {| Part 1 - Zero count: 3 |}]

let%expect_test "part 2 - example simulation" =
  (* Part 2 example: count ALL zero crossings
     L68 from 50: crosses 0 once (50->49->...->0->99->...->82)
     L30 from 82: no zeros
     R48 from 52: lands on 0
     L5  from 0:  no zeros (goes to 95)
     R60 from 95: crosses 0 once (95->96->...->99->0->1->...->55)
     L55 from 55: lands on 0
     L1  from 0:  no zeros
     L99 from 99: lands on 0
     R14 from 0:  no zeros
     L82 from 14: crosses 0 once (14->13->...->0->99->...->32)
     Total: 6 *)
  let result = Day01.simulate_part2 Day01.example_instructions in
  printf "Part 2 - Zero crossings: %d\n" result;
  [%expect {| Part 2 - Zero crossings: 6 |}]

let%expect_test "part 1 - manual position trace" =
  let trace_position instructions =
    let pos = ref 50 in
    let count = ref 0 in
    List.iter instructions ~f:(fun (is_right, dist) ->
      let dist_mod = dist mod 100 in
      let new_pos =
        if is_right then
          (!pos + dist_mod) mod 100
        else
          (!pos - dist_mod + 100) mod 100
      in
      pos := new_pos;
      if new_pos = 0 then incr count;
      printf "pos=%2d\n" new_pos);
    printf "Zero count: %d\n" !count
  in
  trace_position Day01.example_instructions;
  [%expect {|
    pos=82
    pos=52
    pos= 0
    pos=95
    pos=55
    pos= 0
    pos=99
    pos= 0
    pos=14
    pos=32
    Zero count: 3 |}]

let%expect_test "part 2 - manual crossing trace" =
  (* Trace zero crossings manually *)
  let trace_crossings instructions =
    let pos = ref 50 in
    let total = ref 0 in
    List.iter instructions ~f:(fun (is_right, dist) ->
      let crossings =
        if is_right then
          (* Right: (p + d) / 100 *)
          (!pos + dist) / 100
        else
          (* Left: if p = 0 then d/100 else if d >= p then (d-p)/100 + 1 else 0 *)
          if !pos = 0 then dist / 100
          else if dist >= !pos then (dist - !pos) / 100 + 1
          else 0
      in
      total := !total + crossings;
      let new_pos =
        if is_right then (!pos + dist) mod 100
        else (!pos - dist mod 100 + 100) mod 100
      in
      printf "%s%d from %2d -> %2d (crossings: %d)\n"
        (if is_right then "R" else "L") dist !pos new_pos crossings;
      pos := new_pos);
    printf "Total crossings: %d\n" !total
  in
  trace_crossings Day01.example_instructions;
  [%expect {|
    L68 from 50 -> 82 (crossings: 1)
    L30 from 82 -> 52 (crossings: 0)
    R48 from 52 ->  0 (crossings: 1)
    L5 from  0 -> 95 (crossings: 0)
    R60 from 95 -> 55 (crossings: 1)
    L55 from 55 ->  0 (crossings: 1)
    L1 from  0 -> 99 (crossings: 0)
    L99 from 99 ->  0 (crossings: 1)
    R14 from  0 -> 14 (crossings: 0)
    L82 from 14 -> 32 (crossings: 1)
    Total crossings: 6 |}]

let%expect_test "part 2 - R1000 from 50 crosses zero 10 times" =
  (* Special case from problem: R1000 from 50 should cross 0 ten times *)
  let instructions = [ (true, 1000) ] in  (* R1000 *)
  let result = Day01.simulate_part2 instructions in
  printf "R1000 from 50: %d crossings\n" result;
  [%expect {| R1000 from 50: 10 crossings |}]
