(* Sudoku Brute Force Solver in Standard ML *)

val iterations = ref 0

(* Read a matrix file and parse the puzzle *)
fun readMatrixFile filename =
    let
        val file = TextIO.openIn filename

        (* Normalize path for output *)
        val displayPath =
            if String.isPrefix "/app/Matrices/" filename then
                "../" ^ String.extract(filename, 5, NONE)
            else
                filename

        val _ = print (displayPath ^ "\n")

        (* Create 9x9 array initialized to 0 *)
        val puzzle = Array2.array(9, 9, 0)

        (* Read and parse lines *)
        fun readLines row =
            if row >= 9 then ()
            else
                case TextIO.inputLine file of
                    NONE => ()
                  | SOME line =>
                    let
                        val trimmed = String.translate (fn #"\n" => "" | #"\r" => "" | c => str c) line
                    in
                        (* Skip comments and empty lines *)
                        if String.size trimmed = 0 orelse String.sub(trimmed, 0) = #"#" then
                            readLines row
                        else
                            let
                                (* Parse space-separated integers *)
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

(* Print the puzzle *)
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

(* Check if placing val at (row, col) is valid *)
fun isValid puzzle row col value =
    let
        (* Check row *)
        fun checkRow c =
            if c >= 9 then true
            else if Array2.sub(puzzle, row, c) = value then false
            else checkRow (c + 1)

        (* Check column *)
        fun checkCol r =
            if r >= 9 then true
            else if Array2.sub(puzzle, r, col) = value then false
            else checkCol (r + 1)

        (* Check 3x3 box *)
        fun checkBox () =
            let
                val boxRow = (row div 3) * 3
                val boxCol = (col div 3) * 3
                fun checkBoxRow r =
                    if r >= boxRow + 3 then true
                    else
                        let
                            fun checkBoxCol c =
                                if c >= boxCol + 3 then true
                                else if Array2.sub(puzzle, r, c) = value then false
                                else checkBoxCol (c + 1)
                        in
                            if checkBoxCol boxCol then checkBoxRow (r + 1)
                            else false
                        end
            in
                checkBoxRow boxRow
            end
    in
        checkRow 0 andalso checkCol 0 andalso checkBox ()
    end

(* Brute force solver *)
fun solve puzzle =
    let
        (* Find first empty cell (row-major order) *)
        fun findEmpty r c =
            if r >= 9 then NONE
            else if c >= 9 then findEmpty (r + 1) 0
            else if Array2.sub(puzzle, r, c) = 0 then SOME (r, c)
            else findEmpty r (c + 1)

        (* Try values 1-9 for a cell *)
        fun tryValues row col value =
            if value > 9 then false
            else
                let
                    val _ = iterations := !iterations + 1  (* COUNT EVERY ATTEMPT *)
                in
                    if isValid puzzle row col value then
                        let
                            val _ = Array2.update(puzzle, row, col, value)
                        in
                            if solve puzzle then
                                true
                            else
                                (Array2.update(puzzle, row, col, 0);  (* Backtrack *)
                                 tryValues row col (value + 1))
                        end
                    else
                        tryValues row col (value + 1)
                end
    in
        case findEmpty 0 0 of
            NONE =>
                (* No empty cell found - puzzle solved *)
                (printPuzzle puzzle;
                 print ("\nSolved in Iterations=" ^ Int.toString (!iterations) ^ "\n\n");
                 true)
          | SOME (row, col) => tryValues row col 1
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
                    val _ = iterations := 0
                    val _ = solve puzzle
                in
                    ()
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
