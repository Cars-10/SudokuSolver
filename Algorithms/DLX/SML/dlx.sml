(* DLX Sudoku Solver in Standard ML *)

(* Dancing Links node structure using mutable refs *)
datatype dlx_node = DNode of {
    left: int ref,
    right: int ref,
    up: int ref,
    down: int ref,
    column: int ref,
    size: int ref,
    row_id: int,
    col_id: int
}

(* Global iteration counter *)
val dlx_iterations = ref 0

(* Constants *)
val NUM_COLS = 324
val MAX_NODES = 3000  (* Enough for 729 rows * 4 nodes each *)

(* Node pool and counters *)
val nodes = Array.array(MAX_NODES, DNode {
    left = ref 0, right = ref 0, up = ref 0, down = ref 0,
    column = ref 0, size = ref 0, row_id = ~1, col_id = ~1
})
val node_count = ref 0
val root_idx = 0

(* Row info to map back to Sudoku *)
val row_info = Array.array(729, (0, 0, 0))  (* (row, col, num) *)

(* Helper to get node fields *)
fun get_left (DNode {left, ...}) = !left
fun get_right (DNode {right, ...}) = !right
fun get_up (DNode {up, ...}) = !up
fun get_down (DNode {down, ...}) = !down
fun get_column (DNode {column, ...}) = !column
fun get_size (DNode {size, ...}) = !size
fun get_row_id (DNode {row_id, ...}) = row_id
fun get_col_id (DNode {col_id, ...}) = col_id

fun set_left (DNode {left, ...}) v = left := v
fun set_right (DNode {right, ...}) v = right := v
fun set_up (DNode {up, ...}) v = up := v
fun set_down (DNode {down, ...}) v = down := v
fun set_column (DNode {column, ...}) v = column := v
fun inc_size (DNode {size, ...}) = size := !size + 1
fun dec_size (DNode {size, ...}) = size := !size - 1

(* Constraint column calculation *)
fun get_position_col (r, c) = r * 9 + c
fun get_row_col (r, n) = 81 + r * 9 + (n - 1)
fun get_col_col (c, n) = 162 + c * 9 + (n - 1)
fun get_box_col (r, c, n) =
    let val box = (r div 3) * 3 + (c div 3)
    in 243 + box * 9 + (n - 1)
    end

(* Initialize DLX matrix *)
fun init_dlx_matrix () =
    let
        (* Create root node *)
        val root = DNode {
            left = ref NUM_COLS,
            right = ref 1,
            up = ref 0,
            down = ref 0,
            column = ref 0,
            size = ref 0,
            row_id = ~1,
            col_id = 0
        }
        val _ = Array.update(nodes, 0, root)
        val _ = node_count := NUM_COLS + 1

        (* Create column headers (indices 1 to NUM_COLS) *)
        fun init_column i =
            if i > NUM_COLS then ()
            else
                let
                    val col = DNode {
                        left = ref (i - 1),
                        right = ref (if i = NUM_COLS then 0 else i + 1),
                        up = ref i,
                        down = ref i,
                        column = ref i,
                        size = ref 0,
                        row_id = ~1,
                        col_id = i
                    }
                in
                    Array.update(nodes, i, col);
                    init_column (i + 1)
                end
    in
        init_column 1
    end

(* Add a node to column *)
fun add_node col_idx row_id =
    let
        val idx = !node_count
        val _ = if idx >= MAX_NODES then
                    raise Fail "Exceeded maximum node count"
                else ()
        val col = Array.sub(nodes, col_idx)
        val col_up = get_up col

        val node = DNode {
            left = ref 0,
            right = ref 0,
            up = ref col_up,
            down = ref col_idx,
            column = ref col_idx,
            size = ref 0,
            row_id = row_id,
            col_id = col_idx
        }

        val _ = Array.update(nodes, idx, node)
        val _ = node_count := !node_count + 1

        (* Link into column's circular list *)
        val up_node = Array.sub(nodes, col_up)
        val _ = set_down up_node idx
        val _ = set_up col idx
        val _ = inc_size col
    in
        idx
    end

(* Build a DLX row *)
fun build_dlx_row r c n row_id =
    let
        val _ = Array.update(row_info, row_id, (r, c, n))

        (* Create nodes for the 4 constraints *)
        val n1 = add_node (get_position_col (r, c) + 1) row_id
        val n2 = add_node (get_row_col (r, n) + 1) row_id
        val n3 = add_node (get_col_col (c, n) + 1) row_id
        val n4 = add_node (get_box_col (r, c, n) + 1) row_id

        (* Link horizontally *)
        val _ = set_right (Array.sub(nodes, n1)) n2
        val _ = set_right (Array.sub(nodes, n2)) n3
        val _ = set_right (Array.sub(nodes, n3)) n4
        val _ = set_right (Array.sub(nodes, n4)) n1

        val _ = set_left (Array.sub(nodes, n1)) n4
        val _ = set_left (Array.sub(nodes, n2)) n1
        val _ = set_left (Array.sub(nodes, n3)) n2
        val _ = set_left (Array.sub(nodes, n4)) n3
    in
        ()
    end

(* Build matrix from puzzle *)
fun build_dlx_matrix puzzle =
    let
        fun build_cell row_id r c =
            if r >= 9 then row_id
            else if c >= 9 then build_cell row_id (r + 1) 0
            else
                let val cell_val = Array2.sub(puzzle, r, c)
                in
                    if cell_val <> 0 then
                        (build_dlx_row r c cell_val row_id;
                         build_cell (row_id + 1) r (c + 1))
                    else
                        let
                            fun add_possibilities n row_id' =
                                if n > 9 then row_id'
                                else
                                    (build_dlx_row r c n row_id';
                                     add_possibilities (n + 1) (row_id' + 1))
                        in
                            build_cell (add_possibilities 1 row_id) r (c + 1)
                        end
                end
    in
        build_cell 0 0 0;
        ()
    end

(* Cover a column *)
fun cover_column col_idx =
    let
        val col = Array.sub(nodes, col_idx)
        val col_left = get_left col
        val col_right = get_right col

        (* Remove column from header list *)
        val _ = set_right (Array.sub(nodes, col_left)) col_right
        val _ = set_left (Array.sub(nodes, col_right)) col_left

        (* For each row in this column *)
        fun cover_rows row_idx =
            if row_idx = col_idx then ()
            else
                let
                    val row = Array.sub(nodes, row_idx)

                    (* For each node in this row *)
                    fun cover_row_nodes node_idx start =
                        if node_idx = start then ()
                        else
                            let
                                val node = Array.sub(nodes, node_idx)
                                val node_up = get_up node
                                val node_down = get_down node
                                val node_col = get_column node

                                val _ = set_down (Array.sub(nodes, node_up)) node_down
                                val _ = set_up (Array.sub(nodes, node_down)) node_up
                                val _ = dec_size (Array.sub(nodes, node_col))
                                val next_right = get_right node
                            in
                                cover_row_nodes next_right start
                            end

                    val row_right = get_right row
                    val _ = if row_right <> row_idx then
                                cover_row_nodes row_right row_idx
                            else ()
                    val next_down = get_down row
                in
                    cover_rows next_down
                end
    in
        cover_rows (get_down col)
    end

(* Uncover a column *)
fun uncover_column col_idx =
    let
        val col = Array.sub(nodes, col_idx)

        (* For each row in this column (in reverse) *)
        fun uncover_rows row_idx =
            if row_idx = col_idx then ()
            else
                let
                    val row = Array.sub(nodes, row_idx)

                    (* For each node in this row (in reverse) *)
                    fun uncover_row_nodes node_idx start =
                        if node_idx = start then ()
                        else
                            let
                                val node = Array.sub(nodes, node_idx)
                                val node_up = get_up node
                                val node_down = get_down node
                                val node_col = get_column node

                                val _ = inc_size (Array.sub(nodes, node_col))
                                val _ = set_down (Array.sub(nodes, node_up)) node_idx
                                val _ = set_up (Array.sub(nodes, node_down)) node_idx
                                val next_left = get_left node
                            in
                                uncover_row_nodes next_left start
                            end

                    val row_left = get_left row
                    val _ = if row_left <> row_idx then
                                uncover_row_nodes row_left row_idx
                            else ()
                    val next_up = get_up row
                in
                    uncover_rows next_up
                end

        val _ = uncover_rows (get_up col)

        (* Restore column to header list *)
        val col_left = get_left col
        val col_right = get_right col
        val _ = set_right (Array.sub(nodes, col_left)) col_idx
        val _ = set_left (Array.sub(nodes, col_right)) col_idx
    in
        ()
    end

(* Choose column with minimum size *)
fun choose_column () =
    let
        fun find_min col_idx best_idx best_size =
            if col_idx = 0 then best_idx
            else
                let
                    val col = Array.sub(nodes, col_idx)
                    val col_size = get_size col
                    val next_right = get_right col
                in
                    if col_size < best_size then
                        find_min next_right col_idx col_size
                    else
                        find_min next_right best_idx best_size
                end

        val root = Array.sub(nodes, 0)
        val first_col = get_right root
    in
        if first_col = 0 then ~1
        else find_min first_col first_col 999999
    end

(* DLX search *)
fun dlx_search k solution =
    let
        val _ = dlx_iterations := !dlx_iterations + 1
        val root = Array.sub(nodes, 0)
    in
        (* If matrix is empty, solution found *)
        if get_right root = 0 then true
        else
            let
                val col_idx = choose_column ()
            in
                if col_idx = ~1 then false
                else
                    let
                        val col = Array.sub(nodes, col_idx)
                        val col_size = get_size col
                    in
                        if col_size = 0 then false
                        else
                            let
                                val _ = cover_column col_idx

                                fun try_rows row_idx =
                                    if row_idx = col_idx then
                                        (uncover_column col_idx; false)
                                    else
                                        let
                                            val row = Array.sub(nodes, row_idx)
                                            val row_id = get_row_id row
                                            val _ = Array.update(solution, k, row_id)

                                            (* Cover all other columns in this row *)
                                            fun cover_row_cols node_idx start =
                                                if node_idx = start then ()
                                                else
                                                    let
                                                        val node = Array.sub(nodes, node_idx)
                                                        val node_col = get_column node
                                                        val _ = cover_column node_col
                                                        val next_right = get_right node
                                                    in
                                                        cover_row_cols next_right start
                                                    end

                                            val row_right = get_right row
                                            val _ = if row_right <> row_idx then
                                                        cover_row_cols row_right row_idx
                                                    else ()

                                            (* Recurse *)
                                            val found = dlx_search (k + 1) solution
                                        in
                                            if found then
                                                true
                                            else
                                                let
                                                    (* Uncover columns in reverse *)
                                                    fun uncover_row_cols node_idx start =
                                                        if node_idx = start then ()
                                                        else
                                                            let
                                                                val node = Array.sub(nodes, node_idx)
                                                                val node_col = get_column node
                                                                val _ = uncover_column node_col
                                                                val next_left = get_left node
                                                            in
                                                                uncover_row_cols next_left start
                                                            end

                                                    val row_left = get_left row
                                                    val _ = if row_left <> row_idx then
                                                                uncover_row_cols row_left row_idx
                                                            else ()
                                                    val next_down = get_down row
                                                in
                                                    try_rows next_down
                                                end
                                        end
                            in
                                try_rows (get_down col)
                            end
                    end
            end
    end

(* Cover clues *)
fun cover_clues puzzle =
    let
        fun process_cell r c =
            if r >= 9 then ()
            else if c >= 9 then process_cell (r + 1) 0
            else
                let val cell_val = Array2.sub(puzzle, r, c)
                in
                    if cell_val <> 0 then
                        (* Find and cover this row *)
                        let
                            fun find_row row_id =
                                if row_id >= 729 then ()
                                else
                                    let
                                        val (rr, cc, nn) = Array.sub(row_info, row_id)
                                    in
                                        if rr = r andalso cc = c andalso nn = cell_val then
                                            (* Found it - find first node and cover columns *)
                                            let
                                                fun find_node idx =
                                                    if idx >= !node_count then ()
                                                    else
                                                        let val node = Array.sub(nodes, idx)
                                                        in
                                                            if get_row_id node = row_id then
                                                                let
                                                                    fun cover_row_cols node_idx start =
                                                                        let
                                                                            val n = Array.sub(nodes, node_idx)
                                                                            val n_col = get_column n
                                                                            val _ = cover_column n_col
                                                                            val next = get_right n
                                                                        in
                                                                            if next = start then ()
                                                                            else cover_row_cols next start
                                                                        end
                                                                in
                                                                    cover_row_cols idx idx
                                                                end
                                                            else
                                                                find_node (idx + 1)
                                                        end
                                            in
                                                find_node (NUM_COLS + 1)
                                            end
                                        else
                                            find_row (row_id + 1)
                                    end
                        in
                            find_row 0
                        end
                    else ();
                    process_cell r (c + 1)
                end
    in
        process_cell 0 0
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

(* Extract solution *)
fun extract_solution solution solution_len puzzle =
    let
        val result = Array2.array(9, 9, 0)

        (* Copy original puzzle *)
        fun copy_puzzle r =
            if r >= 9 then ()
            else
                let
                    fun copy_col c =
                        if c >= 9 then ()
                        else
                            (Array2.update(result, r, c, Array2.sub(puzzle, r, c));
                             copy_col (c + 1))
                in
                    copy_col 0;
                    copy_puzzle (r + 1)
                end

        val _ = copy_puzzle 0

        (* Apply solution *)
        fun apply_sol i =
            if i >= solution_len then ()
            else
                let
                    val row_id = Array.sub(solution, i)
                in
                    if row_id >= 0 andalso row_id < 729 then
                        let val (r, c, n) = Array.sub(row_info, row_id)
                        in
                            Array2.update(result, r, c, n)
                        end
                    else ();
                    apply_sol (i + 1)
                end

        val _ = apply_sol 0
    in
        result
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

                    (* Reset for new puzzle *)
                    val _ = node_count := 0
                    val _ = init_dlx_matrix ()
                    val _ = build_dlx_matrix puzzle
                    val _ = cover_clues puzzle

                    (* Solve *)
                    val _ = dlx_iterations := 0
                    val solution = Array.array(81, ~1)
                    val found = dlx_search 0 solution
                in
                    if found then
                        let
                            val result = extract_solution solution 81 puzzle
                            val _ = printPuzzle result
                        in
                            print ("\nSolved in Iterations=" ^ Int.toString (!dlx_iterations) ^ "\n\n")
                        end
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
