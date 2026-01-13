// Sudoku DLX Solver - F# Implementation
// Dancing Links (DLX) algorithm using mutable classes for circular doubly-linked lists.
//
// Algorithm:
// - Build exact cover matrix (324 columns for Sudoku constraints)
// - Use Algorithm X with Dancing Links for efficient backtracking
// - Count iterations at start of each search call

open System
open System.Diagnostics
open System.IO

// Type definitions using classes (reference types) to allow circular references
[<AllowNullLiteral>]
type DlxNode() =
    member val Left : DlxNode = null with get, set
    member val Right : DlxNode = null with get, set
    member val Up : DlxNode = null with get, set
    member val Down : DlxNode = null with get, set
    member val Column : DlxColumn = null with get, set
    member val RowId : int = -1 with get, set

and [<AllowNullLiteral>] DlxColumn() =
    member val Node : DlxNode = null with get, set
    member val Size : int = 0 with get, set
    member val Name : string = "" with get, set

// Global state
let mutable dlxIterations = 0
let puzzle = Array2D.zeroCreate<int> 9 9
let solutionGrid = Array2D.zeroCreate<int> 9 9

// Row metadata to map DLX rows back to Sudoku (row, col, num)
type RowInfo = {
    Row: int
    Col: int
    Num: int
}

let rowInfo = Array.zeroCreate<RowInfo> 729
let rowStarts = Array.zeroCreate<DlxNode> 729

// Constraint column index calculations
let getPositionCol r c = r * 9 + c
let getRowCol r n = 81 + r * 9 + (n - 1)
let getColCol c n = 162 + c * 9 + (n - 1)
let getBoxCol r c n =
    let box = (r / 3) * 3 + (c / 3)
    243 + box * 9 + (n - 1)

// Initialize DLX matrix
let initDlxMatrix () =
    // Create root column
    let rootNode = DlxNode()
    let root = DlxColumn(Node = rootNode, Size = 0, Name = "root")
    rootNode.Column <- root
    rootNode.RowId <- -1
    rootNode.Left <- rootNode
    rootNode.Right <- rootNode
    rootNode.Up <- rootNode
    rootNode.Down <- rootNode

    // Create 324 column headers
    let columns = Array.zeroCreate<DlxColumn> 324
    for i in 0 .. 323 do
        let colNode = DlxNode()
        let col = DlxColumn(Node = colNode, Size = 0, Name = sprintf "C%d" i)
        colNode.Column <- col
        colNode.RowId <- -1
        colNode.Up <- colNode
        colNode.Down <- colNode

        // Link into header list
        colNode.Left <- rootNode.Left
        colNode.Right <- rootNode
        rootNode.Left.Right <- colNode
        rootNode.Left <- colNode

        columns.[i] <- col

    (root, columns)

// Add a node to the DLX matrix
let addNode (col: DlxColumn) (rowId: int) =
    let node = DlxNode()
    node.Column <- col
    node.RowId <- rowId

    // Insert at end of column's circular list
    node.Down <- col.Node
    node.Up <- col.Node.Up
    col.Node.Up.Down <- node
    col.Node.Up <- node
    col.Size <- col.Size + 1

    node

// Build a DLX row for Sudoku cell (r,c) with value n
let buildDlxRow (columns: DlxColumn[]) r c n rowId =
    // Store row metadata
    rowInfo.[rowId] <- { Row = r; Col = c; Num = n }

    // Create nodes for the 4 constraints
    let n1 = addNode columns.[getPositionCol r c] rowId
    let n2 = addNode columns.[getRowCol r n] rowId
    let n3 = addNode columns.[getColCol c n] rowId
    let n4 = addNode columns.[getBoxCol r c n] rowId

    // Link nodes horizontally in circular list
    n1.Right <- n2
    n2.Right <- n3
    n3.Right <- n4
    n4.Right <- n1

    n1.Left <- n4
    n2.Left <- n1
    n3.Left <- n2
    n4.Left <- n3

    // Store first node for this row
    rowStarts.[rowId] <- n1

// Build the complete DLX matrix from the puzzle
let buildDlxMatrixFromPuzzle (columns: DlxColumn[]) =
    let mutable rowId = 0
    for r in 0 .. 8 do
        for c in 0 .. 8 do
            if puzzle.[r, c] <> 0 then
                // Cell has a clue - create only one row for that value
                buildDlxRow columns r c puzzle.[r, c] rowId
                rowId <- rowId + 1
            else
                // Cell is empty - create rows for all possible values
                for n in 1 .. 9 do
                    buildDlxRow columns r c n rowId
                    rowId <- rowId + 1

// Cover a column
let rec coverColumn (c: DlxColumn) =
    let colNode = c.Node

    // Remove column header from the header list
    colNode.Right.Left <- colNode.Left
    colNode.Left.Right <- colNode.Right

    // For each row in this column
    let mutable rowNode = colNode.Down
    while not (obj.ReferenceEquals(rowNode, colNode)) do
        // For each node in this row (excluding the column itself)
        let mutable rightNode = rowNode.Right
        while not (obj.ReferenceEquals(rightNode, rowNode)) do
            // Remove this node from its column
            rightNode.Down.Up <- rightNode.Up
            rightNode.Up.Down <- rightNode.Down
            rightNode.Column.Size <- rightNode.Column.Size - 1
            rightNode <- rightNode.Right
        rowNode <- rowNode.Down

// Uncover a column (exact reverse of cover)
let rec uncoverColumn (c: DlxColumn) =
    let colNode = c.Node

    // For each row in this column (in reverse order)
    let mutable rowNode = colNode.Up
    while not (obj.ReferenceEquals(rowNode, colNode)) do
        // For each node in this row (in reverse order)
        let mutable leftNode = rowNode.Left
        while not (obj.ReferenceEquals(leftNode, rowNode)) do
            // Restore this node to its column
            leftNode.Column.Size <- leftNode.Column.Size + 1
            leftNode.Down.Up <- leftNode
            leftNode.Up.Down <- leftNode
            leftNode <- leftNode.Left
        rowNode <- rowNode.Up

    // Restore column header to the header list
    colNode.Right.Left <- colNode
    colNode.Left.Right <- colNode

// Choose column with minimum size (Knuth's S heuristic)
let chooseColumn (root: DlxColumn) =
    let rootNode = root.Node
    let mutable best = None
    let mutable minSize = Int32.MaxValue

    let mutable colNode = rootNode.Right
    while not (obj.ReferenceEquals(colNode, rootNode)) do
        let col = colNode.Column
        if col.Size < minSize then
            minSize <- col.Size
            best <- Some col
        colNode <- colNode.Right

    best

// DLX Search - Algorithm X with Dancing Links
let rec dlxSearch (root: DlxColumn) (k: int) (solution: int[]) =
    dlxIterations <- dlxIterations + 1  // Count every search call

    let rootNode = root.Node

    // If matrix is empty, we found a solution
    if obj.ReferenceEquals(rootNode.Right, rootNode) then
        true
    else
        // Choose column with minimum size
        match chooseColumn root with
        | None -> false
        | Some col ->
            // If column has no rows, no solution possible
            if col.Size = 0 then
                false
            else
                // Cover this column
                coverColumn col

                // Try each row in this column
                let mutable rowNode = col.Node.Down
                let mutable solved = false

                while not (obj.ReferenceEquals(rowNode, col.Node)) && not solved do
                    // Add row to partial solution
                    solution.[k] <- rowNode.RowId

                    // Cover all other columns in this row
                    let mutable rightNode = rowNode.Right
                    while not (obj.ReferenceEquals(rightNode, rowNode)) do
                        coverColumn rightNode.Column
                        rightNode <- rightNode.Right

                    // Recurse
                    if dlxSearch root (k + 1) solution then
                        solved <- true
                    else
                        // Backtrack: uncover all columns in this row
                        let mutable leftNode = rowNode.Left
                        while not (obj.ReferenceEquals(leftNode, rowNode)) do
                            uncoverColumn leftNode.Column
                            leftNode <- leftNode.Left

                    if not solved then
                        rowNode <- rowNode.Down

                // Uncover column
                uncoverColumn col

                solved

// Extract solution from DLX
let extractSolution (solution: int[]) =
    // Start with the original puzzle (includes clues)
    for r in 0 .. 8 do
        for c in 0 .. 8 do
            solutionGrid.[r, c] <- puzzle.[r, c]

    // Each solution entry is a row_id
    for i in 0 .. 80 do
        let rowId = solution.[i]
        if rowId >= 0 && rowId < 729 then
            let info = rowInfo.[rowId]
            solutionGrid.[info.Row, info.Col] <- info.Num

// Cover given clues (pre-selected rows)
let coverClues (root: DlxColumn) =
    for r in 0 .. 8 do
        for c in 0 .. 8 do
            if puzzle.[r, c] <> 0 then
                let n = puzzle.[r, c]

                // Find the row for this clue
                for rowId in 0 .. 728 do
                    if not (isNull rowStarts.[rowId]) &&
                       rowInfo.[rowId].Row = r &&
                       rowInfo.[rowId].Col = c &&
                       rowInfo.[rowId].Num = n then

                        // Cover all columns in this row
                        let node = rowStarts.[rowId]
                        let mutable curr = node
                        let mutable first = true
                        while first || not (obj.ReferenceEquals(curr, node)) do
                            first <- false
                            coverColumn curr.Column
                            curr <- curr.Right

// Print puzzle
let printPuzzle (grid: int[,]) =
    printfn "\nPuzzle:"
    for r in 0 .. 8 do
        for c in 0 .. 8 do
            printf "%d " grid.[r, c]
        printfn ""

// Read matrix file
let readMatrixFile (filename: string) =
    // Normalize path for output
    let displayPath =
        if filename.StartsWith("/app/Matrices/") then
            "../" + filename.Substring(5)
        else
            filename
    printfn "%s" displayPath

    let lines = File.ReadAllLines(filename)
    let mutable lineCount = 0

    for line in lines do
        if lineCount < 9 then
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

[<EntryPoint>]
let main (args: string[]) =
    let stopwatch = Stopwatch.StartNew()

    for arg in args do
        if arg.EndsWith(".matrix") then
            readMatrixFile arg
            printPuzzle puzzle

            // Initialize DLX matrix
            let (root, columns) = initDlxMatrix ()

            // Build matrix from puzzle
            buildDlxMatrixFromPuzzle columns

            // Cover pre-filled clues
            coverClues root

            // Solve using DLX
            dlxIterations <- 0
            let solution = Array.zeroCreate<int> 81
            let result = dlxSearch root 0 solution

            if result then
                extractSolution solution
                printPuzzle solutionGrid
                printfn "\nSolved in Iterations=%d\n" dlxIterations
            else
                printfn "\nNo solution found"

    stopwatch.Stop()
    let elapsed = stopwatch.Elapsed.TotalSeconds
    printfn "Seconds to process %.3f" elapsed
    0
