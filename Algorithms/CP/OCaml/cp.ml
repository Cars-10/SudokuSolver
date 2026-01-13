(* OCaml Constraint Propagation (CP) Sudoku Solver *)
(* Port of C CP implementation using arrays and bitsets *)

(* Grid and candidates (bitsets) *)
let grid = Array.make_matrix 9 9 0
let candidates = Array.make_matrix 9 9 0

(* Global iteration counter *)
let cp_iterations = ref 0

(* Bitset helpers *)
let has_candidate set digit = (set land (1 lsl digit)) <> 0

let remove_candidate set digit = set land (lnot (1 lsl digit))

(* Count number of set bits (popcount) *)
let count_candidates set =
  let rec count n acc =
    if n = 0 then acc
    else count (n land (n - 1)) (acc + 1)
  in
  count set 0

(* Get first candidate digit from bitset (1-9) *)
let get_first_candidate cs =
  let rec find digit =
    if digit > 9 then 0
    else if has_candidate cs digit then digit
    else find (digit + 1)
  in
  find 1

(* Get all 20 peers for a cell (row, col, box) *)
let get_peers row col =
  let peers = ref [] in

  (* Same row (8 cells) *)
  for c = 0 to 8 do
    if c <> col then
      peers := (row, c) :: !peers
  done;

  (* Same column (8 cells) *)
  for r = 0 to 8 do
    if r <> row then
      peers := (r, col) :: !peers
  done;

  (* Same 3x3 box (4 cells not already counted) *)
  let box_row = (row / 3) * 3 in
  let box_col = (col / 3) * 3 in
  for r = box_row to box_row + 2 do
    for c = box_col to box_col + 2 do
      if r <> row && c <> col then
        peers := (r, c) :: !peers
    done
  done;

  List.rev !peers

(* Initialize grid from puzzle *)
let init_grid puzzle =
  for row = 0 to 8 do
    for col = 0 to 8 do
      if puzzle.(row).(col) = 0 then begin
        (* Empty cell: set all candidates 1-9 *)
        grid.(row).(col) <- 0;
        candidates.(row).(col) <- 0x3FE  (* Binary: 0011 1111 1110 (bits 1-9) *)
      end else begin
        (* Given clue: set single value *)
        let digit = puzzle.(row).(col) in
        grid.(row).(col) <- digit;
        candidates.(row).(col) <- (1 lsl digit)
      end
    done
  done

(* Eliminate a digit from a cell's candidates *)
let rec eliminate row col digit =
  (* Check if digit is already eliminated *)
  if not (has_candidate candidates.(row).(col) digit) then
    true
  else begin
    (* Remove digit from candidates *)
    candidates.(row).(col) <- remove_candidate candidates.(row).(col) digit;

    (* Check for contradiction (no candidates left) *)
    let remaining = count_candidates candidates.(row).(col) in
    if remaining = 0 then
      false
    else if remaining = 1 && grid.(row).(col) = 0 then begin
      (* Only one candidate left - assign it (singleton elimination) *)
      let last_digit = get_first_candidate candidates.(row).(col) in
      assign row col last_digit
    end else
      true
  end

(* Assign a digit to a cell *)
and assign row col digit =
  (* Increment iteration counter (benchmark metric) *)
  cp_iterations := !cp_iterations + 1;

  (* Set value *)
  grid.(row).(col) <- digit;
  candidates.(row).(col) <- (1 lsl digit);

  (* Eliminate digit from all peers *)
  let peers = get_peers row col in
  List.for_all (fun (peer_row, peer_col) ->
    eliminate peer_row peer_col digit
  ) peers

(* Constraint propagation *)
let propagate () =
  let changed = ref true in

  while !changed do
    changed := false;

    (* Strategy 1: Singleton elimination *)
    for row = 0 to 8 do
      for col = 0 to 8 do
        if grid.(row).(col) = 0 then begin
          let num_candidates = count_candidates candidates.(row).(col) in
          if num_candidates = 0 then
            raise Exit  (* Contradiction *)
          else if num_candidates = 1 then begin
            let digit = get_first_candidate candidates.(row).(col) in
            if not (assign row col digit) then
              raise Exit;  (* Assignment caused contradiction *)
            changed := true
          end
        end
      done
    done;

    (* Strategy 2: Hidden singles - Check rows *)
    for row = 0 to 8 do
      for digit = 1 to 9 do
        let count = ref 0 in
        let last_col = ref (-1) in
        let already_assigned = ref false in
        for col = 0 to 8 do
          if grid.(row).(col) = digit then begin
            already_assigned := true
          end else if has_candidate candidates.(row).(col) digit then begin
            count := !count + 1;
            last_col := col
          end
        done;
        if not !already_assigned then begin
          if !count = 1 then begin
            if not (assign row !last_col digit) then
              raise Exit;
            changed := true
          end else if !count = 0 then
            raise Exit  (* Digit cannot be placed anywhere in row *)
        end
      done
    done;

    (* Strategy 2: Hidden singles - Check columns *)
    for col = 0 to 8 do
      for digit = 1 to 9 do
        let count = ref 0 in
        let last_row = ref (-1) in
        let already_assigned = ref false in
        for row = 0 to 8 do
          if grid.(row).(col) = digit then begin
            already_assigned := true
          end else if has_candidate candidates.(row).(col) digit then begin
            count := !count + 1;
            last_row := row
          end
        done;
        if not !already_assigned then begin
          if !count = 1 then begin
            if not (assign !last_row col digit) then
              raise Exit;
            changed := true
          end else if !count = 0 then
            raise Exit  (* Digit cannot be placed anywhere in column *)
        end
      done
    done;

    (* Strategy 2: Hidden singles - Check boxes *)
    for box = 0 to 8 do
      let box_row = (box / 3) * 3 in
      let box_col = (box mod 3) * 3 in

      for digit = 1 to 9 do
        let count = ref 0 in
        let last_r = ref (-1) in
        let last_c = ref (-1) in
        let already_assigned = ref false in

        for r = box_row to box_row + 2 do
          for c = box_col to box_col + 2 do
            if grid.(r).(c) = digit then begin
              already_assigned := true
            end else if has_candidate candidates.(r).(c) digit then begin
              count := !count + 1;
              last_r := r;
              last_c := c
            end
          done
        done;

        if not !already_assigned then begin
          if !count = 1 then begin
            if not (assign !last_r !last_c digit) then
              raise Exit;
            changed := true
          end else if !count = 0 then
            raise Exit  (* Digit cannot be placed anywhere in box *)
        end
      done
    done
  done;

  true

(* Find cell with minimum remaining values (MRV heuristic) *)
let find_mrv_cell () =
  let min_candidates = ref 10 in
  let mrv_row = ref (-1) in
  let mrv_col = ref (-1) in

  for r = 0 to 8 do
    for c = 0 to 8 do
      if grid.(r).(c) = 0 then begin
        let num_candidates = count_candidates candidates.(r).(c) in
        if num_candidates < !min_candidates then begin
          min_candidates := num_candidates;
          mrv_row := r;
          mrv_col := c
        end
      end
    done
  done;

  if !mrv_row = -1 then None
  else Some (!mrv_row, !mrv_col)

(* CP search with backtracking *)
let rec cp_search () =
  try
    (* Base case: check if grid is complete *)
    match find_mrv_cell () with
    | None ->
        (* No empty cells - grid is complete *)
        true
    | Some (mrv_row, mrv_col) ->
        (* Recursive case: try each candidate for the MRV cell *)
        let cands = candidates.(mrv_row).(mrv_col) in

        let rec try_digits digit =
          if digit > 9 then
            false
          else if has_candidate cands digit then begin
            (* Save grid state for backtracking *)
            let grid_copy = Array.map Array.copy grid in
            let candidates_copy = Array.map Array.copy candidates in

            try
              (* Try assigning this digit *)
              if assign mrv_row mrv_col digit then begin
                (* Assignment succeeded, propagate constraints *)
                if propagate () then begin
                  (* Propagation succeeded, recurse *)
                  if cp_search () then
                    true
                  else begin
                    (* Failed - restore and try next *)
                    Array.iteri (fun i row -> Array.blit row 0 grid.(i) 0 9) grid_copy;
                    Array.iteri (fun i row -> Array.blit row 0 candidates.(i) 0 9) candidates_copy;
                    try_digits (digit + 1)
                  end
                end else begin
                  (* Propagation failed - restore and try next *)
                  Array.iteri (fun i row -> Array.blit row 0 grid.(i) 0 9) grid_copy;
                  Array.iteri (fun i row -> Array.blit row 0 candidates.(i) 0 9) candidates_copy;
                  try_digits (digit + 1)
                end
              end else begin
                (* Assignment failed - restore and try next *)
                Array.iteri (fun i row -> Array.blit row 0 grid.(i) 0 9) grid_copy;
                Array.iteri (fun i row -> Array.blit row 0 candidates.(i) 0 9) candidates_copy;
                try_digits (digit + 1)
              end
            with Exit ->
              (* Contradiction - restore and try next *)
              Array.iteri (fun i row -> Array.blit row 0 grid.(i) 0 9) grid_copy;
              Array.iteri (fun i row -> Array.blit row 0 candidates.(i) 0 9) candidates_copy;
              try_digits (digit + 1)
          end else
            try_digits (digit + 1)
        in
        try_digits 1
  with Exit -> false

(* Print puzzle *)
let print_puzzle g =
  Printf.printf "\nPuzzle:\n";
  for r = 0 to 8 do
    for c = 0 to 8 do
      Printf.printf "%d " g.(r).(c)
    done;
    Printf.printf "\n"
  done

(* Print raw board *)
let print_raw_board g =
  for r = 0 to 8 do
    for c = 0 to 8 do
      Printf.printf "%d " g.(r).(c)
    done;
    Printf.printf "\n"
  done

(* Read matrix file *)
let read_matrix_file filename =
  try
    Printf.printf "%s\n" filename;

    let ic = open_in filename in
    let puzzle = Array.make_matrix 9 9 0 in
    let row = ref 0 in

    try
      while !row < 9 do
        let line = input_line ic in
        let trimmed = String.trim line in
        if String.length trimmed > 0 && trimmed.[0] <> '#' then begin
          let parts = String.split_on_char ' ' trimmed
                     |> List.filter (fun s -> s <> "") in
          if List.length parts = 9 then begin
            List.iteri (fun col s ->
              puzzle.(!row).(col) <- int_of_string s
            ) parts;
            row := !row + 1
          end
        end
      done;
      close_in ic;
      Some puzzle
    with End_of_file ->
      close_in ic;
      if !row = 9 then Some puzzle else None
  with e ->
    Printf.fprintf stderr "Error reading file %s: %s\n" filename (Printexc.to_string e);
    None

(* Main solver *)
let solve_puzzle filename =
  match read_matrix_file filename with
  | None -> ()
  | Some puzzle ->
      (* Print initial board *)
      print_raw_board puzzle;
      print_puzzle puzzle;

      (* Initialize grid *)
      init_grid puzzle;

      (* Solve *)
      cp_iterations := 0;

      try
        (* Initial propagation for given clues *)
        if propagate () then begin
          if cp_search () then begin
            print_puzzle grid;
            Printf.printf "\nSolved in Iterations=%d\n\n" !cp_iterations
          end else
            Printf.printf "No solution found\n"
        end else
          Printf.printf "No solution found (initial propagation failed)\n"
      with Exit ->
        Printf.printf "No solution found (contradiction)\n"

(* Main *)
let () =
  let start_time = Unix.gettimeofday () in

  (* Process each .matrix file from command line *)
  for i = 1 to Array.length Sys.argv - 1 do
    let filename = Sys.argv.(i) in
    if String.length filename >= 7 &&
       String.sub filename (String.length filename - 7) 7 = ".matrix" then
      solve_puzzle filename
  done;

  let end_time = Unix.gettimeofday () in
  Printf.printf "Seconds to process %.3f\n" (end_time -. start_time)
