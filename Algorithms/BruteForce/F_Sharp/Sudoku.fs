// Sudoku Solver - F# Implementation
// Brute-force backtracking algorithm matching C reference exactly.
//
// Algorithm:
// - Row-major search for empty cells (top-to-bottom, left-to-right)
// - Try values 1-9 in ascending order
// - Count EVERY placement attempt (algorithm fingerprint)

open System
open System.Diagnostics
open System.IO

let mutable puzzle = Array2D.zeroCreate<int> 9 9
let mutable count = 0

let printPuzzle () =
    printfn "\nPuzzle:"
    for row in 0 .. 8 do
        for col in 0 .. 8 do
            printf "%d " puzzle.[row, col]
        printfn ""

let readMatrixFile (filename: string) =
    // Normalize path for output (match C format)
    let displayPath =
        if filename.StartsWith("/app/Matrices/") then
            "../" + filename.Substring(5)
        else
            filename
    printfn "%s" displayPath

    puzzle <- Array2D.zeroCreate<int> 9 9
    let lines = File.ReadAllLines(filename)
    let mutable lineCount = 0

    for line in lines do
        if lineCount >= 9 then ()
        else
            let trimmed = line.Trim()
            // Skip comments and empty lines
            if not (String.IsNullOrEmpty(trimmed)) && not (trimmed.StartsWith("#")) then
                let parts = trimmed.Split([| ' '; '\t' |], StringSplitOptions.RemoveEmptyEntries)
                if parts.Length = 9 then
                    for j in 0 .. 8 do
                        puzzle.[lineCount, j] <- Int32.Parse(parts.[j])
                        printf "%d " puzzle.[lineCount, j]
                    printfn ""
                    lineCount <- lineCount + 1

let isValid (row: int) (col: int) (value: int) =
    // Check row
    let mutable valid = true
    for i in 0 .. 8 do
        if puzzle.[row, i] = value then valid <- false

    // Check column
    if valid then
        for i in 0 .. 8 do
            if puzzle.[i, col] = value then valid <- false

    // Check 3x3 box
    if valid then
        let boxRow = (row / 3) * 3
        let boxCol = (col / 3) * 3
        for i in 0 .. 2 do
            for j in 0 .. 2 do
                if puzzle.[boxRow + i, boxCol + j] = value then valid <- false

    valid

let rec solve () =
    // Find first empty cell (row-major order)
    let mutable row = -1
    let mutable col = -1
    let mutable found = false

    for r in 0 .. 8 do
        if not found then
            for c in 0 .. 8 do
                if not found && puzzle.[r, c] = 0 then
                    row <- r
                    col <- c
                    found <- true

    // If no empty cell found, puzzle is solved
    if row = -1 then
        printPuzzle ()
        printfn "\nSolved in Iterations=%d\n" count
        true
    else
        // Try values 1-9 in order
        let mutable solved = false
        for value in 1 .. 9 do
            if not solved then
                count <- count + 1  // COUNT EVERY ATTEMPT - algorithm fingerprint

                if isValid row col value then
                    puzzle.[row, col] <- value  // Place value

                    if solve () then
                        solved <- true
                    else
                        puzzle.[row, col] <- 0  // Backtrack
        solved

[<EntryPoint>]
let main (args: string[]) =
    let stopwatch = Stopwatch.StartNew()

    for arg in args do
        if arg.EndsWith(".matrix") then
            readMatrixFile arg
            printPuzzle ()
            count <- 0
            solve () |> ignore

    stopwatch.Stop()
    let elapsed = stopwatch.Elapsed.TotalSeconds
    printfn "Seconds to process %.3f" elapsed
    0
