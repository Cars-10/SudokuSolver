let puzzle = Array.make_matrix 9 9 0
let iterations = ref 0

let print_board () =
  print_endline "Puzzle:";
  for i = 0 to 8 do
    for j = 0 to 8 do
      Printf.printf "%d " puzzle.(i).(j)
    done;
    print_newline ()
  done

let is_possible row col num =
  let possible = ref true in
  for i = 0 to 8 do
    if puzzle.(row).(i) = num || puzzle.(i).(col) = num then possible := false
  done;
  if !possible then (
    let start_row = (row / 3) * 3 in
    let start_col = (col / 3) * 3 in
    for i = 0 to 2 do
      for j = 0 to 2 do
        if puzzle.(start_row + i).(start_col + j) = num then possible := false
      done
    done
  );
  !possible

let rec solve row col =
  if row = 9 then true
  else
    let next_row, next_col =
      if col = 8 then (row + 1, 0) else (row, col + 1)
    in
    if puzzle.(row).(col) <> 0 then solve next_row next_col
    else
      let rec try_num num =
        if num > 9 then false
        else if (
          incr iterations;
          is_possible row col num
        ) then (
          puzzle.(row).(col) <- num;
          if solve next_row next_col then true
          else (
            puzzle.(row).(col) <- 0;
            try_num (num + 1)
          )
        ) else try_num (num + 1)
      in
      try_num 1

let read_board filename =
  try
    let ic = open_in filename in
    let rec read_lines row =
      try
        let line = input_line ic in
        let trimmed = String.trim line in
        if String.length trimmed > 0 && trimmed.[0] <> '#' then (
          let parts = String.split_on_char ' ' trimmed |> List.filter (fun s -> s <> "") in
          List.iteri (fun col s ->
            if col < 9 then puzzle.(row).(col) <- int_of_string s
          ) parts;
          if row < 8 then read_lines (row + 1)
        ) else read_lines row
      with End_of_file -> ()
    in
    read_lines 0;
    close_in ic;
    true
  with e ->
    Printf.printf "Error reading file %s: %s\n" filename (Printexc.to_string e);
    false

let () =
  let args = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  if Array.length args = 0 then
    print_endline "Usage: ./Sudoku <file1> <file2> ..."
  else
    Array.iter (fun filename ->
      Printf.printf "\nProcessing %s\n" filename;
      if read_board filename then (
        print_board ();
        iterations := 0;
        if solve 0 0 then (
          print_board ();
          Printf.printf "\nSolved in Iterations=%d\n" !iterations
        ) else
          print_endline "No solution found"
      )
    ) args
