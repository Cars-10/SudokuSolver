(* OCaml Sudoku Solver - Brute Force Algorithm *)
(* Matches C reference implementation exactly *)

let puzzle = Array.make_matrix 9 9 0
let count = ref 0

(* Print puzzle in formatted style *)
let print_puzzle () =
  Printf.printf "\nPuzzle:\n";
  for row = 0 to 8 do
    for col = 0 to 8 do
      Printf.printf "%d " puzzle.(row).(col)
    done;
    Printf.printf "\n"
  done

(* Print raw board values (matches C output format) *)
let print_raw_board () =
  for row = 0 to 8 do
    for col = 0 to 8 do
      Printf.printf "%d " puzzle.(row).(col)
    done;
    Printf.printf "\n"
  done

(* Check if placing val at (row, col) is valid *)
let is_valid row col value =
  (* Check row *)
  let row_ok = ref true in
  for i = 0 to 8 do
    if puzzle.(row).(i) = value then row_ok := false
  done;

  (* Check column *)
  let col_ok = ref true in
  for i = 0 to 8 do
    if puzzle.(i).(col) = value then col_ok := false
  done;

  (* Check 3x3 box *)
  let box_ok = ref true in
  let box_row = (row / 3) * 3 in
  let box_col = (col / 3) * 3 in
  for i = 0 to 2 do
    for j = 0 to 2 do
      if puzzle.(box_row + i).(box_col + j) = value then box_ok := false
    done
  done;

  !row_ok && !col_ok && !box_ok

(* Find first empty cell (row-major order) *)
let find_empty () =
  let result = ref None in
  try
    for row = 0 to 8 do
      for col = 0 to 8 do
        if puzzle.(row).(col) = 0 then (
          result := Some (row, col);
          raise Exit
        )
      done
    done;
    !result
  with Exit -> !result

(* BRUTE-FORCE SOLVER *)
(* Searches row-major order (top-to-bottom, left-to-right) *)
(* Tries candidates 1-9 in ascending order *)
(* Counts EVERY placement attempt (the algorithm fingerprint) *)
let rec solve () =
  match find_empty () with
  | None ->
      (* No empty cell found, puzzle is solved *)
      print_puzzle ();
      Printf.printf "\nSolved in Iterations=%d\n\n" !count;
      true
  | Some (row, col) ->
      (* Try values 1-9 in order *)
      let rec try_values value =
        if value > 9 then false
        else begin
          count := !count + 1;  (* COUNT EVERY ATTEMPT - this is the algorithm fingerprint *)

          if is_valid row col value then begin
            puzzle.(row).(col) <- value;  (* Place value *)

            if solve () then true  (* Solved *)
            else begin
              puzzle.(row).(col) <- 0;  (* Backtrack *)
              try_values (value + 1)
            end
          end else
            try_values (value + 1)
        end
      in
      try_values 1

(* Read matrix file *)
let read_matrix_file filename =
  try
    (* Print filename *)
    Printf.printf "%s\n" filename;

    let ic = open_in filename in
    let row = ref 0 in
    try
      while !row < 9 do
        let line = input_line ic in
        let trimmed = String.trim line in
        (* Skip comments and empty lines *)
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
      true
    with End_of_file ->
      close_in ic;
      !row = 9
  with e ->
    Printf.fprintf stderr "Error reading file %s: %s\n" filename (Printexc.to_string e);
    false

(* Main *)
let () =
  let start_time = Unix.gettimeofday () in

  (* Process each .matrix file from command line *)
  for i = 1 to Array.length Sys.argv - 1 do
    let filename = Sys.argv.(i) in
    if String.length filename >= 7 &&
       String.sub filename (String.length filename - 7) 7 = ".matrix" then begin
      if read_matrix_file filename then begin
        (* Print raw board *)
        print_raw_board ();
        print_puzzle ();
        count := 0;
        let _ = solve () in
        ()
      end
    end
  done;

  let end_time = Unix.gettimeofday () in
  Printf.printf "Seconds to process %.3f\n" (end_time -. start_time)
