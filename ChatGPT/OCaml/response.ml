(* OCaml code for a Sudoku solver *

(* Step 1: Read a 9x9 matrix from a file into a 1D array, ignoring comments *)
let read_matrix filename =
  let ch = open_in filename in
  let matrix = Array.make 81 0 in
  let rec read_line i =
    if i < 81 then
      try
        let line = input_line ch in
        if String.get line 0 <> '#' then begin
          let nums = List.map int_of_string (String.split_on_char ',' (String.trim line)) in
          List.iteri (fun j n -> matrix.(i + j) <- n) nums;
          read_line (i + 9)
        end else
          read_line i
      with End_of_file -> ()
    else
      ()
  in
  read_line 0;
  close_in ch;
  matrix

(* Step 2: Calculate the complexity of a 9x9 matrix *)
let calculate_complexity matrix =
  Array.fold_left (fun acc x -> if x = 0 then acc + 1 else acc) 0 matrix

(* Step 3: Print the board in a 9x9 grid and the complexity for the unsolved board *)
let print_board matrix =
  Printf.printf "Complexity: %d\n" (calculate_complexity matrix);
  Array.iteri (fun i x ->
    Printf.printf "%d ";
    if (i + 1) mod 9 = 0 then Printf.printf "\n") matrix

(* Step 4: Solve the Sudoku board using a backtracking algorithm *)
let solve_board matrix =
  let is_valid n i =
    let row = i / 9 and col = i mod 9 in
    let block_row = row / 3 * 3 and block_col = col / 3 * 3 in
    for j = 0 to 8 do
      let row_idx = 9 * row + j in
      let col_idx = col + 9 * j in
      let block_idx = block_row * 9 + block_col + (j / 3) * 9 + (j mod 3) in
      if matrix.(row_idx) = n || matrix.(col_idx) = n || matrix.(block_idx) = n then
        return false
    done;
    true
  in
  let rec backtrack i count =
    if i = 81 then
      (true, count)
    else if matrix.(i) <> 0 then
      backtrack (i + 1) count
    else
      let rec try_num n count =
        if n = 10 then
          (false, count)
        else if is_valid n i then
          begin
            matrix.(i) <- n;
            let (solved, count) = backtrack (i + 1) (count + 1) in
            if solved then
              (true, count)
            else
              (matrix.(i) <- 0; try_num (n + 1) count)
          end
        else
          try_num (n + 1) count
      in
      try_num 1 count
  in
  let (solved, count) = backtrack 0 0 in
  (solved, matrix, count)

(* Step 5: Print the final board and the number of iterations with commas for thousands *)
let print_solution (solved, matrix, count) =
  if solved then begin
    print_board matrix;
    Printf.printf "Solved with %s iterations\n" (String.concat "," (List.map string_of_int (List.rev (Utils.int_to_list count))))
  end else
    Printf.printf "No solution found\n"

(* Step 6: Read matrices from the command line and solve them *)
let () =
  let filename = Sys.argv.(1) in
  let matrix = read_matrix filename in
  print_board matrix;
  let solution = solve_board matrix in
  print_solution solution

(* Helper function to convert an integer to a list of its digits *)
module Utils = struct
  let rec int_to_list n =
    if n = 0 then [] else int_to_list (n / 10) @ [n mod 10]
end
)