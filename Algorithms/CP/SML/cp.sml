(* Constraint Propagation Sudoku Solver in Standard ML *)

(* Global iteration counter *)
val cp_iterations = ref 0

(* Bitset operations using Word *)
fun has_candidate (set: Word.word) (digit: int) =
    Word.andb(set, Word.<<(0w1, Word.fromInt digit)) <> 0w0

fun remove_candidate (set: Word.word) (digit: int) =
    Word.andb(set, Word.notb(Word.<<(0w1, Word.fromInt digit)))

fun count_candidates (set: Word.word) =
    let
        fun count n w =
            if w = 0w0 then n
            else count (n + (if Word.andb(w, 0w1) = 0w1 then 1 else 0)) (Word.>>(w, 0w1))
    in
        count 0 set
    end

fun get_first_candidate (set: Word.word) =
    let
        fun find digit =
            if digit > 9 then 0
            else if has_candidate set digit then digit
            else find (digit + 1)
    in
        find 1
    end

(* Get all 20 peers for a cell *)
fun get_peers row col =
    let
        (* Same row (8 cells) *)
        val row_peers = List.tabulate(9, fn c => (row, c))
        val row_peers_filtered = List.filter (fn (_, c) => c <> col) row_peers

        (* Same column (8 cells) *)
        val col_peers = List.tabulate(9, fn r => (r, col))
        val col_peers_filtered = List.filter (fn (r, _) => r <> row) col_peers

        (* Same 3x3 box (4 cells) *)
        val box_row = (row div 3) * 3
        val box_col = (col div 3) * 3
        val box_peers = List.concat(List.tabulate(3, fn dr =>
            List.tabulate(3, fn dc =>
                (box_row + dr, box_col + dc))))
        val box_peers_filtered = List.filter (fn (r, c) =>
            not (r = row andalso c = col) andalso
            not (r = row orelse c = col)) box_peers
    in
        row_peers_filtered @ col_peers_filtered @ box_peers_filtered
    end

(* Grid structure *)
type grid = {
    values: int Array2.array,
    candidates: Word.word Array2.array
}

(* Initialize grid *)
fun init_grid puzzle =
    let
        val values = Array2.array(9, 9, 0)
        val candidates = Array2.array(9, 9, 0w0)

        fun init_cell r c =
            if r >= 9 then ()
            else if c >= 9 then init_cell (r + 1) 0
            else
                let val cell_val = Array2.sub(puzzle, r, c)
                in
                    if cell_val = 0 then
                        (Array2.update(values, r, c, 0);
                         Array2.update(candidates, r, c, 0w1022); (* 0x3FE = bits 1-9 *)
                         init_cell r (c + 1))
                    else
                        (Array2.update(values, r, c, cell_val);
                         Array2.update(candidates, r, c, Word.<<(0w1, Word.fromInt cell_val));
                         init_cell r (c + 1))
                end
    in
        init_cell 0 0;
        {values = values, candidates = candidates}
    end

(* Forward declaration using mutual recursion *)
fun eliminate (grid: grid) row col digit =
    let
        val {values, candidates} = grid
        val current = Array2.sub(candidates, row, col)
    in
        (* Check if already eliminated *)
        if not (has_candidate current digit) then
            true
        else
            let
                val new_cands = remove_candidate current digit
                val _ = Array2.update(candidates, row, col, new_cands)
                val remaining = count_candidates new_cands
            in
                (* Contradiction check *)
                if remaining = 0 then
                    false
                (* Singleton elimination *)
                else if remaining = 1 andalso Array2.sub(values, row, col) = 0 then
                    let val last_digit = get_first_candidate new_cands
                    in
                        assign grid row col last_digit
                    end
                else
                    true
            end
    end

and assign (grid: grid) row col digit =
    let
        val {values, candidates} = grid
        val _ = cp_iterations := !cp_iterations + 1
        val _ = Array2.update(values, row, col, digit)
        val _ = Array2.update(candidates, row, col, Word.<<(0w1, Word.fromInt digit))

        (* Eliminate from peers *)
        val peers = get_peers row col

        fun eliminate_from_peers [] = true
          | eliminate_from_peers ((pr, pc)::rest) =
            if eliminate grid pr pc digit then
                eliminate_from_peers rest
            else
                false
    in
        eliminate_from_peers peers
    end

(* Propagate constraints *)
fun propagate (grid: grid) =
    let
        val {values, candidates} = grid
        val changed = ref true
        val result = ref true

        fun propagate_loop () =
            if not (!changed) orelse not (!result) then !result
            else
                let
                    val _ = changed := false

                    (* Strategy 1: Singleton elimination *)
                    fun singleton_elim r c =
                        if r >= 9 then ()
                        else if c >= 9 then singleton_elim (r + 1) 0
                        else if Array2.sub(values, r, c) = 0 then
                            let
                                val cands = Array2.sub(candidates, r, c)
                                val num_cands = count_candidates cands
                            in
                                if num_cands = 0 then
                                    result := false
                                else if num_cands = 1 then
                                    let
                                        val digit = get_first_candidate cands
                                    in
                                        if assign grid r c digit then
                                            changed := true
                                        else
                                            result := false
                                    end
                                else ();
                                singleton_elim r (c + 1)
                            end
                        else
                            singleton_elim r (c + 1)

                    val _ = singleton_elim 0 0

                    (* Strategy 2: Hidden singles in rows *)
                    fun hidden_singles_rows r =
                        if r >= 9 orelse not (!result) then ()
                        else
                            let
                                fun check_digit digit =
                                    if digit > 9 then ()
                                    else
                                        let
                                            fun count_in_row c count last_c =
                                                if c >= 9 then (count, last_c)
                                                else if Array2.sub(values, r, c) = digit then
                                                    (0, ~1)  (* Already assigned *)
                                                else if has_candidate (Array2.sub(candidates, r, c)) digit then
                                                    count_in_row (c + 1) (count + 1) c
                                                else
                                                    count_in_row (c + 1) count last_c

                                            val (count, last_col) = count_in_row 0 0 ~1
                                        in
                                            if count = 1 then
                                                if assign grid r last_col digit then
                                                    changed := true
                                                else
                                                    result := false
                                            else ();
                                            check_digit (digit + 1)
                                        end
                            in
                                check_digit 1;
                                hidden_singles_rows (r + 1)
                            end

                    val _ = hidden_singles_rows 0

                    (* Strategy 3: Hidden singles in columns *)
                    fun hidden_singles_cols c =
                        if c >= 9 orelse not (!result) then ()
                        else
                            let
                                fun check_digit digit =
                                    if digit > 9 then ()
                                    else
                                        let
                                            fun count_in_col r count last_r =
                                                if r >= 9 then (count, last_r)
                                                else if Array2.sub(values, r, c) = digit then
                                                    (0, ~1)  (* Already assigned *)
                                                else if has_candidate (Array2.sub(candidates, r, c)) digit then
                                                    count_in_col (r + 1) (count + 1) r
                                                else
                                                    count_in_col (r + 1) count last_r

                                            val (count, last_row) = count_in_col 0 0 ~1
                                        in
                                            if count = 1 then
                                                if assign grid last_row c digit then
                                                    changed := true
                                                else
                                                    result := false
                                            else ();
                                            check_digit (digit + 1)
                                        end
                            in
                                check_digit 1;
                                hidden_singles_cols (c + 1)
                            end

                    val _ = hidden_singles_cols 0

                    (* Strategy 4: Hidden singles in boxes *)
                    fun hidden_singles_boxes box =
                        if box >= 9 orelse not (!result) then ()
                        else
                            let
                                val box_row = (box div 3) * 3
                                val box_col = (box mod 3) * 3

                                fun check_digit digit =
                                    if digit > 9 then ()
                                    else
                                        let
                                            fun count_in_box dr count last_r last_c =
                                                if dr >= 3 then (count, last_r, last_c)
                                                else
                                                    let
                                                        fun count_in_box_row dc count last_r last_c =
                                                            if dc >= 3 then (count, last_r, last_c)
                                                            else
                                                                let
                                                                    val r = box_row + dr
                                                                    val c = box_col + dc
                                                                in
                                                                    if Array2.sub(values, r, c) = digit then
                                                                        (0, ~1, ~1)  (* Already assigned *)
                                                                    else if has_candidate (Array2.sub(candidates, r, c)) digit then
                                                                        count_in_box_row (dc + 1) (count + 1) r c
                                                                    else
                                                                        count_in_box_row (dc + 1) count last_r last_c
                                                                end

                                                        val (cnt, lr, lc) = count_in_box_row 0 count last_r last_c
                                                    in
                                                        if cnt = 0 then (0, ~1, ~1)
                                                        else count_in_box (dr + 1) cnt lr lc
                                                    end

                                            val (count, last_row, last_col) = count_in_box 0 0 ~1 ~1
                                        in
                                            if count = 1 then
                                                if assign grid last_row last_col digit then
                                                    changed := true
                                                else
                                                    result := false
                                            else ();
                                            check_digit (digit + 1)
                                        end
                            in
                                check_digit 1;
                                hidden_singles_boxes (box + 1)
                            end

                    val _ = hidden_singles_boxes 0
                in
                    propagate_loop ()
                end
    in
        propagate_loop ()
    end

(* Find MRV cell *)
fun find_mrv_cell (grid: grid) =
    let
        val {values, candidates} = grid

        fun find_min r c best_r best_c best_count =
            if r >= 9 then
                if best_r = ~1 then NONE
                else SOME (best_r, best_c)
            else if c >= 9 then
                find_min (r + 1) 0 best_r best_c best_count
            else if Array2.sub(values, r, c) = 0 then
                let
                    val num_cands = count_candidates (Array2.sub(candidates, r, c))
                in
                    if num_cands < best_count then
                        find_min r (c + 1) r c num_cands
                    else
                        find_min r (c + 1) best_r best_c best_count
                end
            else
                find_min r (c + 1) best_r best_c best_count
    in
        find_min 0 0 ~1 ~1 10
    end

(* Copy grid *)
fun copy_grid (grid: grid) =
    let
        val {values, candidates} = grid
        val new_values = Array2.array(9, 9, 0)
        val new_candidates = Array2.array(9, 9, 0w0)

        fun copy_cells r c =
            if r >= 9 then ()
            else if c >= 9 then copy_cells (r + 1) 0
            else
                (Array2.update(new_values, r, c, Array2.sub(values, r, c));
                 Array2.update(new_candidates, r, c, Array2.sub(candidates, r, c));
                 copy_cells r (c + 1))
    in
        copy_cells 0 0;
        {values = new_values, candidates = new_candidates}
    end

(* CP search with backtracking *)
fun cp_search (grid: grid) =
    case find_mrv_cell grid of
        NONE =>
            (* No empty cells - solved *)
            let
                val {values, ...} = grid
                val solution = Array.array(81, 0)

                fun extract r c =
                    if r >= 9 then ()
                    else if c >= 9 then extract (r + 1) 0
                    else
                        (Array.update(solution, r * 9 + c, Array2.sub(values, r, c));
                         extract r (c + 1))
            in
                extract 0 0;
                SOME solution
            end
      | SOME (mrv_row, mrv_col) =>
            let
                val {candidates, ...} = grid
                val cands = Array2.sub(candidates, mrv_row, mrv_col)

                fun try_digit digit =
                    if digit > 9 then NONE
                    else if has_candidate cands digit then
                        let
                            val grid_copy = copy_grid grid
                        in
                            if assign grid_copy mrv_row mrv_col digit then
                                if propagate grid_copy then
                                    case cp_search grid_copy of
                                        SOME sol => SOME sol
                                      | NONE => try_digit (digit + 1)
                                else
                                    try_digit (digit + 1)
                            else
                                try_digit (digit + 1)
                        end
                    else
                        try_digit (digit + 1)
            in
                try_digit 1
            end

(* Read matrix file *)
fun readMatrixFile filename =
    let
        val file = TextIO.openIn filename
        val displayPath =
            if String.isPrefix "/app/Matrices/" filename then
                "../" ^ String.extract(filename, 5, NONE)
            else
                filename
        val _ = print (displayPath ^ "\n")
        val puzzle = Array2.array(9, 9, 0)

        fun readLines row =
            if row >= 9 then ()
            else
                case TextIO.inputLine file of
                    NONE => ()
                  | SOME line =>
                    let
                        val trimmed = String.translate (fn #"\n" => "" | #"\r" => "" | c => str c) line
                    in
                        if String.size trimmed = 0 orelse String.sub(trimmed, 0) = #"#" then
                            readLines row
                        else
                            let
                                val tokens = String.tokens Char.isSpace trimmed
                                val numbers = List.mapPartial Int.fromString tokens
                            in
                                if List.length numbers = 9 then
                                    let
                                        fun fillRow col nums =
                                            case (col, nums) of
                                                (_, []) => ()
                                              | (c, n::ns) =>
                                                (Array2.update(puzzle, row, c, n);
                                                 print (Int.toString n ^ " ");
                                                 fillRow (c + 1) ns)
                                    in
                                        fillRow 0 numbers;
                                        print "\n";
                                        readLines (row + 1)
                                    end
                                else
                                    readLines row
                            end
                    end
    in
        readLines 0;
        TextIO.closeIn file;
        puzzle
    end

(* Print puzzle *)
fun printPuzzle puzzle =
    let
        fun printRow row =
            if row >= 9 then ()
            else
                let
                    fun printCol col =
                        if col >= 9 then print "\n"
                        else
                            (print (Int.toString (Array2.sub(puzzle, row, col)) ^ " ");
                             printCol (col + 1))
                in
                    printCol 0;
                    printRow (row + 1)
                end
    in
        print "\nPuzzle:\n";
        printRow 0
    end

(* Print solution from array *)
fun printSolution solution =
    let
        fun printRow row =
            if row >= 9 then ()
            else
                let
                    fun printCol col =
                        if col >= 9 then print "\n"
                        else
                            (print (Int.toString (Array.sub(solution, row * 9 + col)) ^ " ");
                             printCol (col + 1))
                in
                    printCol 0;
                    printRow (row + 1)
                end
    in
        print "\nPuzzle:\n";
        printRow 0
    end

(* Main function *)
fun main () =
    let
        val args = CommandLine.arguments ()
        val startTime = Time.now ()

        fun processFile filename =
            if String.isSuffix ".matrix" filename then
                let
                    val puzzle = readMatrixFile filename
                    val _ = printPuzzle puzzle

                    (* Initialize grid - this sets up clues directly in init_grid *)
                    val grid = init_grid puzzle

                    (* Reset iteration counter AFTER init_grid *)
                    val _ = cp_iterations := 0

                    (* Initial propagation for clues *)
                    val prop_ok = propagate grid
                in
                    if prop_ok then
                        case cp_search grid of
                            SOME solution =>
                                (printSolution solution;
                                 print ("\nSolved in Iterations=" ^ Int.toString (!cp_iterations) ^ "\n\n"))
                          | NONE =>
                                print "\nNo solution found\n"
                    else
                        print "\nNo solution found\n"
                end
            else
                ()

        val _ = List.app processFile args

        val endTime = Time.now ()
        val duration = Time.toReal (Time.-(endTime, startTime))
    in
        print ("Seconds to process " ^ Real.fmt (StringCvt.FIX (SOME 3)) duration ^ "\n")
    end

val _ = main ()
