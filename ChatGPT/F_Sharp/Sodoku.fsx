open System
open System.IO
open System.Diagnostics

let size = 9
let mutable iterationCount = 0

let isSafe (grid: int[,]) (row: int) (col: int) (num: int) : bool =
    let squareStartRow = row / 3 * 3
    let squareStartCol = col / 3 * 3
    let inRow = [0 .. size - 1] |> Seq.exists (fun x -> grid.[row, x] = num)
    let inCol = [0 .. size - 1] |> Seq.exists (fun x -> grid.[x, col] = num)
    let inSquare = [0 .. 2] |> Seq.exists (fun i -> [0 .. 2] |> Seq.exists (fun j -> grid.[squareStartRow + i, squareStartCol + j] = num))
    not (inRow || inCol || inSquare)


let rec solveSudoku (grid: int[,]) (row: int) (col: int) : bool =
    iterationCount <- iterationCount + 1
    if row = size then true
    elif col = size then solveSudoku grid (row + 1) 0
    elif grid.[row, col] > 0 then solveSudoku grid row (col + 1)
    else
        [1 .. size]
        |> List.exists (fun num ->
            if isSafe grid row col num then
                grid.[row, col] <- num
                let result = solveSudoku grid row (col + 1)
                if not result then grid.[row, col] <- 0
                result
            else false)

let printGrid (grid: int[,]) =
    for row in 0 .. size - 1 do
        for col in 0 .. size - 1 do
            printf "%d " grid.[row, col]
        printfn ""

let loadSudokuGrid (path: string) : int[,] =
    let grid = Array2D.zeroCreate<int> size size
    let lines = File.ReadLines(path)
    let mutable row = 0
    printfn "Loading Sudoku grid from: %s" path
    for line in lines do
        if not (String.IsNullOrWhiteSpace(line) || line.StartsWith("#")) then
            printfn "Reading line: %s" line
            try
                let values = line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                                 |> Array.map (fun v -> v.Trim())
                                 |> Array.filter (fun v -> v <> "")
                if values.Length <> size then
                    printfn "Warning: Expected %d values in row, but found %d" size values.Length
                let intValues = Array.map Int32.Parse values
                if intValues.Length = size then
                    for col in 0 .. size - 1 do
                        grid.[row, col] <- intValues.[col]
                        printf " %d" grid.[row, col]
                    printfn "" // End of row
                    row <- row + 1
                else
                    printfn "Error: Incorrect number of columns in row. Expected %d, got %d." size intValues.Length
            with
            | :? FormatException as e ->
                printfn "Error parsing line: %s" e.Message
                raise e
    if row <> size then
        printfn "Error: Incorrect number of rows. Expected %d, got %d." size row
        raise (FormatException "The number of rows in the grid does not match the expected size of the Sudoku grid.")
    grid



let timeSudokuSolver (paths: string[]) =
    let stopwatch = Stopwatch.StartNew()
    for path in paths do
        iterationCount <- 0
        let grid = loadSudokuGrid path
        if solveSudoku grid 0 0 then
            printfn "Sudoku Grid Solved with %d iterations:" iterationCount
            printGrid grid
        else
            printfn "No solution exists for the provided Sudoku grid in file: %s" path
        printfn ""
    stopwatch.Stop()
    let elapsed = stopwatch.Elapsed.TotalSeconds
    printfn "Execution time: %.2f seconds" elapsed

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        printfn "Usage: sudoku_solver.exe <file1> [<file2> ...]"
        1
    else
        timeSudokuSolver argv
        0
