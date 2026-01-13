(* OCaml Dancing Links (DLX) Sudoku Solver *)
(* Port of C DLX implementation using mutable record fields *)

(* Type definitions - mutable record fields allow circular references *)
type dlx_node = {
  mutable left: dlx_node;
  mutable right: dlx_node;
  mutable up: dlx_node;
  mutable down: dlx_node;
  mutable column: dlx_node;
  mutable size: int;
  row_id: int;
  col_id: int;
}

(* Global iteration counter *)
let dlx_iterations = ref 0

(* Create a dummy node to serve as root/column header *)
let rec make_node row_id col_id =
  let rec node = {
    left = node;
    right = node;
    up = node;
    down = node;
    column = node;
    size = 0;
    row_id = row_id;
    col_id = col_id;
  } in
  node

(* Cover a column in the DLX matrix *)
let cover_column col =
  (* Remove column header from the header list *)
  col.right.left <- col.left;
  col.left.right <- col.right;

  (* For each row in this column *)
  let rec cover_rows row_node =
    if row_node != col then begin
      (* For each node in this row *)
      let rec cover_row_nodes right_node =
        if right_node != row_node then begin
          (* Remove this node from its column *)
          right_node.down.up <- right_node.up;
          right_node.up.down <- right_node.down;
          right_node.column.size <- right_node.column.size - 1;
          cover_row_nodes right_node.right
        end
      in
      cover_row_nodes row_node.right;
      cover_rows row_node.down
    end
  in
  cover_rows col.down

(* Uncover a column (exact reverse of cover) *)
let uncover_column col =
  (* For each row in this column (in reverse order) *)
  let rec uncover_rows row_node =
    if row_node != col then begin
      (* For each node in this row (in reverse order) *)
      let rec uncover_row_nodes left_node =
        if left_node != row_node then begin
          (* Restore this node to its column *)
          left_node.column.size <- left_node.column.size + 1;
          left_node.down.up <- left_node;
          left_node.up.down <- left_node;
          uncover_row_nodes left_node.left
        end
      in
      uncover_row_nodes row_node.left;
      uncover_rows row_node.up
    end
  in
  uncover_rows col.up;

  (* Restore column header to the header list *)
  col.right.left <- col;
  col.left.right <- col

(* Choose column with minimum size (Knuth's S heuristic) *)
let choose_column root =
  let min_col = ref None in
  let min_size = ref max_int in

  let rec scan col_node =
    if col_node != root then begin
      if col_node.size < !min_size then begin
        min_size := col_node.size;
        min_col := Some col_node
      end;
      scan col_node.right
    end
  in
  scan root.right;
  !min_col

(* DLX Search - Algorithm X with Dancing Links *)
let rec dlx_search root k solution =
  dlx_iterations := !dlx_iterations + 1;

  (* If matrix is empty, we found a solution *)
  if root.right == root then
    true
  else begin
    (* Choose column with minimum size *)
    match choose_column root with
    | None -> false
    | Some col ->
        (* If column has no rows, no solution possible *)
        if col.size = 0 then
          false
        else begin
          (* Cover this column *)
          cover_column col;

          (* Try each row in this column *)
          let rec try_rows row_node =
            if row_node == col then begin
              (* Uncover column and backtrack *)
              uncover_column col;
              false
            end else begin
              (* Add row to partial solution *)
              solution.(k) <- row_node.row_id;

              (* Cover all other columns in this row *)
              let rec cover_cols right_node =
                if right_node != row_node then begin
                  cover_column right_node.column;
                  cover_cols right_node.right
                end
              in
              cover_cols row_node.right;

              (* Recurse *)
              if dlx_search root (k + 1) solution then
                true
              else begin
                (* Backtrack: uncover all columns in this row *)
                let rec uncover_cols left_node =
                  if left_node != row_node then begin
                    uncover_column left_node.column;
                    uncover_cols left_node.left
                  end
                in
                uncover_cols row_node.left;

                try_rows row_node.down
              end
            end
          in
          try_rows col.down
        end
  end

(* Build DLX matrix for Sudoku *)
let build_dlx_matrix puzzle =
  (* Create root node *)
  let root = make_node (-1) (-1) in

  (* Create 324 column headers for Sudoku constraints *)
  (* 81 cell constraints + 81 row constraints + 81 col constraints + 81 box constraints *)
  let num_cols = 324 in
  let columns = Array.init num_cols (fun i -> make_node (-1) i) in

  (* Link column headers horizontally *)
  for i = 0 to num_cols - 1 do
    let col = columns.(i) in
    col.left <- (if i = 0 then root else columns.(i - 1));
    col.right <- (if i = num_cols - 1 then root else columns.(i + 1));
    if i = 0 then root.right <- col;
    if i = num_cols - 1 then root.left <- col;

    (* Set column as its own column header *)
    col.column <- col
  done;

  (* Store row metadata: map row_id to (r, c, digit) *)
  let row_metadata = Array.make 729 (0, 0, 0) in
  let row_first_nodes = Array.make 729 None in

  (* For each possible placement (row, col, digit) *)
  let row_count = ref 0 in
  for r = 0 to 8 do
    for c = 0 to 8 do
      if puzzle.(r).(c) <> 0 then begin
        (* Cell has a clue - create only one row for that value *)
        let d = puzzle.(r).(c) in
        let row_id = !row_count in
        row_metadata.(row_id) <- (r, c, d);

        (* Create 4 nodes for this placement (one per constraint type) *)
        let cell_col = r * 9 + c in
        let row_col = 81 + r * 9 + (d - 1) in
        let col_col = 162 + c * 9 + (d - 1) in
        let box_col = 243 + ((r / 3) * 3 + (c / 3)) * 9 + (d - 1) in

        let node1 = make_node row_id cell_col in
        let node2 = make_node row_id row_col in
        let node3 = make_node row_id col_col in
        let node4 = make_node row_id box_col in

        row_first_nodes.(row_id) <- Some node1;

        (* Link nodes horizontally in a circular list *)
        node1.right <- node2;
        node2.right <- node3;
        node3.right <- node4;
        node4.right <- node1;
        node1.left <- node4;
        node2.left <- node1;
        node3.left <- node2;
        node4.left <- node3;

        (* Insert each node into its column *)
        let insert_into_column node col_idx =
          let col = columns.(col_idx) in
          node.column <- col;
          node.up <- col.up;
          node.down <- col;
          col.up.down <- node;
          col.up <- node;
          col.size <- col.size + 1
        in

        insert_into_column node1 cell_col;
        insert_into_column node2 row_col;
        insert_into_column node3 col_col;
        insert_into_column node4 box_col;

        row_count := !row_count + 1
      end else begin
        (* Cell is empty - create rows for all possible values 1-9 *)
        for d = 1 to 9 do
          let row_id = !row_count in
          row_metadata.(row_id) <- (r, c, d);

          (* Create 4 nodes for this placement (one per constraint type) *)
          let cell_col = r * 9 + c in
          let row_col = 81 + r * 9 + (d - 1) in
          let col_col = 162 + c * 9 + (d - 1) in
          let box_col = 243 + ((r / 3) * 3 + (c / 3)) * 9 + (d - 1) in

          let node1 = make_node row_id cell_col in
          let node2 = make_node row_id row_col in
          let node3 = make_node row_id col_col in
          let node4 = make_node row_id box_col in

          row_first_nodes.(row_id) <- Some node1;

          (* Link nodes horizontally in a circular list *)
          node1.right <- node2;
          node2.right <- node3;
          node3.right <- node4;
          node4.right <- node1;
          node1.left <- node4;
          node2.left <- node1;
          node3.left <- node2;
          node4.left <- node3;

          (* Insert each node into its column *)
          let insert_into_column node col_idx =
            let col = columns.(col_idx) in
            node.column <- col;
            node.up <- col.up;
            node.down <- col;
            col.up.down <- node;
            col.up <- node;
            col.size <- col.size + 1
          in

          insert_into_column node1 cell_col;
          insert_into_column node2 row_col;
          insert_into_column node3 col_col;
          insert_into_column node4 box_col;

          row_count := !row_count + 1
        done
      end
    done
  done;

  (root, !row_count, row_metadata, row_first_nodes)

(* Decode solution from row IDs to Sudoku grid *)
let decode_solution solution num_rows puzzle row_metadata =
  let grid = Array.make_matrix 9 9 0 in

  (* Copy given clues *)
  for r = 0 to 8 do
    for c = 0 to 8 do
      if puzzle.(r).(c) <> 0 then
        grid.(r).(c) <- puzzle.(r).(c)
    done
  done;

  (* Decode each solution row using metadata *)
  for i = 0 to num_rows - 1 do
    let row_id = solution.(i) in
    if row_id >= 0 && row_id < Array.length row_metadata then begin
      let (r, c, d) = row_metadata.(row_id) in
      grid.(r).(c) <- d
    end
  done;

  grid

(* Cover given clues before starting search *)
let cover_clues puzzle row_metadata row_first_nodes =
  for r = 0 to 8 do
    for c = 0 to 8 do
      if puzzle.(r).(c) <> 0 then begin
        let d = puzzle.(r).(c) in
        (* Find the row for this clue *)
        let found = ref false in
        for row_id = 0 to Array.length row_metadata - 1 do
          if not !found then begin
            let (mr, mc, md) = row_metadata.(row_id) in
            match row_first_nodes.(row_id) with
            | Some first_node when mr = r && mc = c && md = d ->
                (* Cover all columns in this row *)
                let rec cover_row_cols node =
                  cover_column node.column;
                  if node.right != first_node then
                    cover_row_cols node.right
                in
                cover_row_cols first_node;
                found := true
            | _ -> ()
          end
        done
      end
    done
  done

(* Print puzzle *)
let print_puzzle grid =
  Printf.printf "\nPuzzle:\n";
  for r = 0 to 8 do
    for c = 0 to 8 do
      Printf.printf "%d " grid.(r).(c)
    done;
    Printf.printf "\n"
  done

(* Print raw board *)
let print_raw_board grid =
  for r = 0 to 8 do
    for c = 0 to 8 do
      Printf.printf "%d " grid.(r).(c)
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

      (* Build DLX matrix *)
      let (root, max_rows, row_metadata, row_first_nodes) = build_dlx_matrix puzzle in

      (* Cover clues before solving *)
      cover_clues puzzle row_metadata row_first_nodes;

      (* Solve *)
      dlx_iterations := 0;
      let solution = Array.make 81 (-1) in

      if dlx_search root 0 solution then begin
        (* Decode and print solution *)
        let solved = decode_solution solution 81 puzzle row_metadata in
        print_puzzle solved;
        Printf.printf "\nSolved in Iterations=%d\n\n" !dlx_iterations
      end else begin
        Printf.printf "No solution found\n"
      end

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
