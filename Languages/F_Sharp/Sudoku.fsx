open System
open System.IO

let mutable iterations = 0

let printPuzzle (puzzle: int[,]) = 
    printfn "\nPuzzle:"
    for j in 0 .. 8 do
        for i in 0 .. 8 do
            printf "%i " puzzle.[j,i]
        printfn ""

let isValid (puzzle: int[,]) (row: int) (col: int) (num: int) =
    let mutable valid = true
    // Check row and column
    for i in 0 .. 8 do
        if puzzle.[row, i] = num || puzzle.[i, col] = num then
            valid <- false
    
    // Check 3x3 subgrid
    if valid then
        let startRow = (row / 3) * 3
        let startCol = (col / 3) * 3
        for i in 0 .. 2 do
            for j in 0 .. 2 do
                if puzzle.[startRow + i, startCol + j] = num then
                    valid <- false
    valid

let rec solve (puzzle: int[,]) (row: int) (col: int) : bool =
    if row = 9 then
        true
    else
        let nextRow, nextCol = 
            if col = 8 then (row + 1, 0)
            else (row, col + 1)
        
        if puzzle.[row, col] <> 0 then
            solve puzzle nextRow nextCol
        else
            let mutable found = false
            let mutable num = 1
            while not found && num <= 9 do
                iterations <- iterations + 1
                if isValid puzzle row col num then
                    puzzle.[row, col] <- num
                    if solve puzzle nextRow nextCol then
                        found <- true
                    else
                        puzzle.[row, col] <- 0
                num <- num + 1
            found

let readMatrixFile (filename : String) =
    if not(File.Exists (filename))  then
        printfn "File does not exist: %s" filename 
        exit 1
    
    let puzzle = Array2D.create 9 9 0
    let lines = System.IO.File.ReadAllLines(filename)
    let mutable j = 0
    for line in lines do
        if not(String.IsNullOrWhiteSpace(line)) && not(line.StartsWith("#")) then
            let mutable i = 0
            for value in line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) do
                if i < 9 then
                    puzzle.[j,i] <- value |> int
                    i <- i + 1
            j <- j + 1
    puzzle

let main (args: string[]) =
    for item in args do
        if item.EndsWith(".matrix") then
            printfn "\nProcessing %s" item
            try
                let puzzle = readMatrixFile item
                printPuzzle puzzle
                
                iterations <- 0
                if solve puzzle 0 0 then
                    printPuzzle puzzle
                    printfn "\nSolved in Iterations=%d" iterations
                else
                    printfn "No solution found"
            with
                | ex -> printfn "Error: %s" ex.Message
    0

// Invoke main if running as a script
main (System.Environment.GetCommandLineArgs())
