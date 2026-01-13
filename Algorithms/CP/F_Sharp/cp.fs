// Sudoku CP Solver - F# Implementation
// Constraint Propagation algorithm with MRV (Minimum Remaining Values) heuristic.
//
// Algorithm:
// - Use bitsets for candidate tracking (bits 1-9)
// - Apply constraint propagation: singleton elimination + hidden singles
// - Search using MRV heuristic
// - Count iterations at each assignment

open System
open System.Diagnostics
open System.IO

// Type definitions
type CPGrid = {
    mutable Values: int[,]       // 9x9 grid of assigned values (0 = empty)
    mutable Candidates: int[,]   // 9x9 grid of candidate bitsets
}

// Global state
let mutable cpIterations = 0L
let puzzle = Array2D.zeroCreate<int> 9 9
let solutionGrid = Array.zeroCreate<int> 81

// Bitset helpers
let hasCandidate (set: int) (digit: int) = (set &&& (1 <<< digit)) <> 0
let removeCandidate (set: int) (digit: int) = set &&& ~~~(1 <<< digit)
let addCandidate (set: int) (digit: int) = set ||| (1 <<< digit)

let rec countCandidates (set: int) =
    if set = 0 then 0
    else countCandidates (set &&& (set - 1)) + 1

let getFirstCandidate (set: int) =
    let mutable result = 0
    for digit in 1 .. 9 do
        if result = 0 && hasCandidate set digit then
            result <- digit
    result

// Get all 20 peers for a cell (row, col, box)
let getPeers row col =
    let peers = ResizeArray<int * int>()

    // Same row (8 cells)
    for c in 0 .. 8 do
        if c <> col then
            peers.Add((row, c))

    // Same column (8 cells)
    for r in 0 .. 8 do
        if r <> row then
            peers.Add((r, col))

    // Same box (4 cells - minus already counted)
    let boxRow = (row / 3) * 3
    let boxCol = (col / 3) * 3
    for r in boxRow .. boxRow + 2 do
        for c in boxCol .. boxCol + 2 do
            if r <> row && c <> col then
                peers.Add((r, c))

    peers |> Seq.toArray

// Initialize grid from puzzle
let initGrid () =
    let values = Array2D.zeroCreate<int> 9 9
    let candidates = Array2D.create 9 9 0

    for row in 0 .. 8 do
        for col in 0 .. 8 do
            if puzzle.[row, col] = 0 then
                // Empty cell: set all candidates 1-9
                values.[row, col] <- 0
                candidates.[row, col] <- 0x3FE  // Binary: 0011 1111 1110 (bits 1-9)
            else
                // Given clue: set single value
                let digit = puzzle.[row, col]
                values.[row, col] <- digit
                candidates.[row, col] <- (1 <<< digit)

    { Values = values; Candidates = candidates }

// Eliminate a digit from a cell's candidates (no iteration counting)
let rec eliminateNoCount (grid: CPGrid) row col digit =
    // Check if digit is already eliminated
    if not (hasCandidate grid.Candidates.[row, col] digit) then
        true  // Already eliminated, no change
    else
        // Remove digit from candidates
        grid.Candidates.[row, col] <- removeCandidate grid.Candidates.[row, col] digit

        // Check for contradiction (no candidates left)
        let remaining = countCandidates grid.Candidates.[row, col]
        if remaining = 0 then
            false  // Contradiction
        else
            true

// Initialize constraints for clues (eliminate clue digits from peers without counting)
let initClueConstraints (grid: CPGrid) =
    let mutable success = true
    for row in 0 .. 8 do
        for col in 0 .. 8 do
            if success && grid.Values.[row, col] <> 0 then
                let digit = grid.Values.[row, col]
                let peers = getPeers row col
                for (peerRow, peerCol) in peers do
                    if success then
                        success <- eliminateNoCount grid peerRow peerCol digit
    success

// Eliminate a digit from a cell's candidates
let rec eliminate (grid: CPGrid) row col digit =
    // Check if digit is already eliminated
    if not (hasCandidate grid.Candidates.[row, col] digit) then
        true  // Already eliminated, no change
    else
        // Remove digit from candidates
        grid.Candidates.[row, col] <- removeCandidate grid.Candidates.[row, col] digit

        // Check for contradiction (no candidates left)
        let remaining = countCandidates grid.Candidates.[row, col]
        if remaining = 0 then
            false  // Contradiction
        elif remaining = 1 && grid.Values.[row, col] = 0 then
            // Only one candidate left, assign it (singleton elimination)
            let lastDigit = getFirstCandidate grid.Candidates.[row, col]
            assign grid row col lastDigit
        else
            true

// Assign a digit to a cell
and assign (grid: CPGrid) row col digit =
    // Increment iteration counter (benchmark metric)
    cpIterations <- cpIterations + 1L

    // Set value
    grid.Values.[row, col] <- digit
    grid.Candidates.[row, col] <- (1 <<< digit)

    // Eliminate digit from all peers
    let peers = getPeers row col
    let mutable success = true
    let mutable i = 0

    while i < peers.Length && success do
        let (peerRow, peerCol) = peers.[i]
        success <- eliminate grid peerRow peerCol digit
        i <- i + 1

    success

// Propagate constraints until fixpoint
let propagate (grid: CPGrid) =
    let mutable changed = true
    let mutable success = true

    while changed && success do
        changed <- false

        // Strategy 1: Singleton elimination
        // Check for cells with only one candidate
        for row in 0 .. 8 do
            for col in 0 .. 8 do
                if success && grid.Values.[row, col] = 0 then
                    let numCandidates = countCandidates grid.Candidates.[row, col]
                    if numCandidates = 0 then
                        success <- false  // Contradiction
                    elif numCandidates = 1 then
                        let digit = getFirstCandidate grid.Candidates.[row, col]
                        if assign grid row col digit then
                            changed <- true
                        else
                            success <- false

        // Strategy 2: Hidden singles
        // For each unit (row, col, box), if a digit appears in only one cell, assign it

        // Check rows
        if success then
            for row in 0 .. 8 do
                for digit in 1 .. 9 do
                    if success then
                        let mutable count = 0
                        let mutable lastCol = -1
                        let mutable alreadyAssigned = false

                        for col in 0 .. 8 do
                            if grid.Values.[row, col] = digit then
                                alreadyAssigned <- true

                        if not alreadyAssigned then
                            for col in 0 .. 8 do
                                if hasCandidate grid.Candidates.[row, col] digit then
                                    count <- count + 1
                                    lastCol <- col

                            if count = 1 then
                                if assign grid row lastCol digit then
                                    changed <- true
                                else
                                    success <- false
                            elif count = 0 then
                                success <- false  // Digit cannot be placed anywhere

        // Check columns
        if success then
            for col in 0 .. 8 do
                for digit in 1 .. 9 do
                    if success then
                        let mutable count = 0
                        let mutable lastRow = -1
                        let mutable alreadyAssigned = false

                        for row in 0 .. 8 do
                            if grid.Values.[row, col] = digit then
                                alreadyAssigned <- true

                        if not alreadyAssigned then
                            for row in 0 .. 8 do
                                if hasCandidate grid.Candidates.[row, col] digit then
                                    count <- count + 1
                                    lastRow <- row

                            if count = 1 then
                                if assign grid lastRow col digit then
                                    changed <- true
                                else
                                    success <- false
                            elif count = 0 then
                                success <- false  // Digit cannot be placed anywhere

        // Check boxes
        if success then
            for box in 0 .. 8 do
                let boxRow = (box / 3) * 3
                let boxCol = (box % 3) * 3

                for digit in 1 .. 9 do
                    if success then
                        let mutable count = 0
                        let mutable lastR = -1
                        let mutable lastC = -1
                        let mutable alreadyAssigned = false

                        for r in boxRow .. boxRow + 2 do
                            for c in boxCol .. boxCol + 2 do
                                if grid.Values.[r, c] = digit then
                                    alreadyAssigned <- true

                        if not alreadyAssigned then
                            for r in boxRow .. boxRow + 2 do
                                for c in boxCol .. boxCol + 2 do
                                    if hasCandidate grid.Candidates.[r, c] digit then
                                        count <- count + 1
                                        lastR <- r
                                        lastC <- c

                            if count = 1 then
                                if assign grid lastR lastC digit then
                                    changed <- true
                                else
                                    success <- false
                            elif count = 0 then
                                success <- false  // Digit cannot be placed anywhere

    success

// Find cell with minimum remaining values (MRV heuristic)
let findMRVCell (grid: CPGrid) =
    let mutable minCandidates = 10
    let mutable bestRow = -1
    let mutable bestCol = -1

    for row in 0 .. 8 do
        for col in 0 .. 8 do
            if grid.Values.[row, col] = 0 then
                let numCandidates = countCandidates grid.Candidates.[row, col]
                if numCandidates < minCandidates then
                    minCandidates <- numCandidates
                    bestRow <- row
                    bestCol <- col

    if bestRow = -1 then None
    else Some (bestRow, bestCol)

// Copy grid for backtracking
let copyGrid (grid: CPGrid) =
    {
        Values = Array2D.copy grid.Values
        Candidates = Array2D.copy grid.Candidates
    }

// CP Search with backtracking
let rec cpSearch (grid: CPGrid) =
    // Base case: check if grid is complete
    match findMRVCell grid with
    | None ->
        // No empty cells - grid is complete
        for r in 0 .. 8 do
            for c in 0 .. 8 do
                solutionGrid.[r * 9 + c] <- grid.Values.[r, c]
        true

    | Some (mrvRow, mrvCol) ->
        // Recursive case: try each candidate for the MRV cell
        let candidates = grid.Candidates.[mrvRow, mrvCol]
        let mutable solved = false

        for digit in 1 .. 9 do
            if not solved && hasCandidate candidates digit then
                // Save grid state for backtracking
                let gridCopy = copyGrid grid

                // Try assigning this digit
                if assign grid mrvRow mrvCol digit then
                    // Assignment succeeded, propagate constraints
                    if propagate grid then
                        // Propagation succeeded, recurse
                        if cpSearch grid then
                            solved <- true

                // Failed - restore grid state and try next candidate
                if not solved then
                    grid.Values <- gridCopy.Values
                    grid.Candidates <- gridCopy.Candidates

        solved

// Print puzzle
let printPuzzle (grid: int[,]) =
    printfn "\nPuzzle:"
    for r in 0 .. 8 do
        for c in 0 .. 8 do
            printf "%d " grid.[r, c]
        printfn ""

// Print solution from flat array
let printSolution () =
    printfn "\nPuzzle:"
    for r in 0 .. 8 do
        for c in 0 .. 8 do
            printf "%d " solutionGrid.[r * 9 + c]
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

            // Initialize grid (clues are set, but peers not yet eliminated - like C version)
            let grid = initGrid ()

            // Apply initial propagation (iterations from this point will be counted)
            // This will eliminate clue digits from peers and trigger counted assignments
            cpIterations <- 0L
            let success = propagate grid

            // Solve using CP search
            if success && cpSearch grid then
                printSolution ()
                printfn "\nSolved in Iterations=%d\n" cpIterations
            else
                printfn "\nNo solution found"

    stopwatch.Stop()
    let elapsed = stopwatch.Elapsed.TotalSeconds
    printfn "Seconds to process %.3f" elapsed
    0
