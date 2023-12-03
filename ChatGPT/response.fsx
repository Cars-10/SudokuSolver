open System
open System.IO
open System.Globalization

// Step 1: Read a 9x9 space-separated matrix from a file and return as a 1D array
let readSudokuBoard (filename: string) =
    let lines = File.ReadAllLines filename
    let validLines = lines |> Array.filter (fun line -> not (line.StartsWith "#"))
    let numbers = validLines |> Array.collect (fun line -> line.Split ' ' |> Array.map int)
    numbers

// Step 2: Calculate the complexity of a 9x9 matrix
let calculateComplexity (board: int[]) =
    board |> Array.fold (fun acc n -> if n = 0 then acc + 1 else acc) 0

// Step 3: Print the board in a 9x9 grid
let printBoard (board: int[]) =
    board
    |> Array.mapi (fun i n -> if i % 9 = 0 && i <> 0 then sprintf "\n%d" n else sprintf "%d " n)
    |> (fun numbers -> String.concat "" numbers |> printf "%s\n")

// Step 4: Solve the Sudoku board using a backtracking algorithm
let solveSudoku (board: int[]) =
    let mutable iterations = 0L
    let isSafe board number pos =
        let row = pos / 9
        let col = pos % 9
        let boxRow = row / 3 * 3
        let boxCol = col / 3 * 3
        let rowSafe = not (Array.exists (fun n -> n = number) [| for i in 0..8 -> board.[row * 9 + i] |])
        let colSafe = not (Array.exists (fun n -> n = number) [| for i in 0..8 -> board.[i * 9 + col] |])
        let boxSafe = not (Array.exists (fun n -> n = number)
                                        [| for i in 0..2 do
                                             for j in 0..2 do
                                                 yield board.[(boxRow + i) * 9 + (boxCol + j)] |])
        rowSafe && colSafe && boxSafe
    let rec backtrack pos =
        if pos = 81 then true
        else if board.[pos] <> 0 then backtrack (pos + 1)
        else
            [1..9] |> List.exists (fun number ->
                if isSafe board number pos then
                    iterations <- iterations + 1L
                    board.[pos] <- number
                    if backtrack (pos + 1) then true
                    else
                        board.[pos] <- 0
                        false
                else false)
    if backtrack 0 then (true, iterations)
    else (false, iterations)

// Step 5: When solved print the final board and the number of iterations with formatting
let printSolution (board: int[]) (iterations: int64) =
    printBoard board
    printfn "Iterations: %s" (iterations.ToString("N0", CultureInfo.InvariantCulture))

// Step 6: Accept matrices from the commandline, print the complexity before solving
[<EntryPoint>]
let main argv =
    if argv.Length <> 1 then
        printfn "Usage: SudokuSolver <input_file>"
        1
    else
        let board = readSudokuBoard argv.[0]
        printfn "Initial Sudoku Board:"
        printBoard board
        let complexity = calculateComplexity board
        printfn "Complexity: %d" complexity
        let (solved, iterations) = solveSudoku board
        if solved then
            printfn "Solved Sudoku Board:"
            printSolution board iterations
            0
        else
            printfn "Failed to solve Sudoku Board."
            1
