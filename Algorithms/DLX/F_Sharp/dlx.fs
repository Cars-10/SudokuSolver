
open System
open System.IO
open System.Diagnostics

// Types
[<AllowNullLiteral>]
type DlxNode() =
    member val Up: DlxNode = null with get, set
    member val Down: DlxNode = null with get, set
    member val Left: DlxNode = null with get, set
    member val Right: DlxNode = null with get, set
    member val Column: DlxColumn = null with get, set
    member val RowId: int = -1 with get, set

and [<AllowNullLiteral>]
    DlxColumn() =
    inherit DlxNode()
    member val Size: int = 0 with get, set
    member val Name: string = "" with get, set
    new(n: string) as this = 
        DlxColumn() then
        this.Name <- n
        this.Column <- this

type RowInfo = {
    Row: int
    Col: int
    Num: int
}

// Global state
let mutable dlxIterations = 0
let mutable root: DlxColumn = null
let mutable columns: DlxColumn[] = null
let mutable nodes: DlxNode[] = null
let mutable nodeCount = 0
let mutable rowInfo: RowInfo[] = null
let mutable rowStarts: DlxNode[] = null
let mutable puzzle: int[,] = Array2D.create 9 9 0
let mutable solutionGrid: int[,] = Array2D.create 9 9 0

let maxNodes = 729 * 4

// --- DLX Logic ---

let initDlxMatrix () =
    root <- new DlxColumn("root")
    root.Left <- root
    root.Right <- root
    root.Up <- root
    root.Down <- root
    
    columns <- Array.zeroCreate 324
    for i in 0 .. 323 do
        let col = new DlxColumn(sprintf "C%d" i)
        columns.[i] <- col
        col.Up <- col
        col.Down <- col
        
        // Link to header
        col.Left <- root.Left
        col.Right <- root
        root.Left.Right <- col
        root.Left <- col

    nodes <- Array.zeroCreate maxNodes
    for i in 0 .. maxNodes - 1 do
        nodes.[i] <- new DlxNode()
    
    nodeCount <- 0
    rowInfo <- Array.zeroCreate 729
    rowStarts <- Array.zeroCreate 729

let addNode (col: DlxColumn) (rowId: int) =
    if nodeCount >= maxNodes then failwith "Max nodes exceeded"
    
    let node = nodes.[nodeCount]
    nodeCount <- nodeCount + 1
    
    node.Column <- col
    node.RowId <- rowId
    
    // Insert at bottom
    node.Down <- col
    node.Up <- col.Up
    col.Up.Down <- node
    col.Up <- node
    col.Size <- col.Size + 1
    
    node

let getPositionCol r c = r * 9 + c
let getRowCol r n = 81 + r * 9 + (n - 1)
let getColCol c n = 162 + c * 9 + (n - 1)
let getBoxCol r c n =
    let box = (r / 3) * 3 + (c / 3)
    243 + box * 9 + (n - 1)

let buildDlxRow r c n id =
    rowInfo.[id] <- { Row = r; Col = c; Num = n }
    
    let n1 = addNode columns.[getPositionCol r c] id
    let n2 = addNode columns.[getRowCol r n] id
    let n3 = addNode columns.[getColCol c n] id
    let n4 = addNode columns.[getBoxCol r c n] id
    
    n1.Right <- n2; n2.Right <- n3; n3.Right <- n4; n4.Right <- n1
    n1.Left <- n4; n2.Left <- n1; n3.Left <- n2; n4.Left <- n3
    
    rowStarts.[id] <- n1

let buildMatrix () =
    let mutable id = 0
    for r in 0 .. 8 do
        for c in 0 .. 8 do
            if puzzle.[r,c] <> 0 then
                buildDlxRow r c puzzle.[r,c] id
                id <- id + 1
            else
                for n in 1 .. 9 do
                    buildDlxRow r c n id
                    id <- id + 1

let cover (c: DlxColumn) =
    c.Right.Left <- c.Left
    c.Left.Right <- c.Right
    
    let mutable row = c.Down
    while row <> (c :> DlxNode) do
        let mutable right = row.Right
        while right <> row do
            right.Down.Up <- right.Up
            right.Up.Down <- right.Down
            right.Column.Size <- right.Column.Size - 1
            right <- right.Right
        row <- row.Down

let uncover (c: DlxColumn) =
    let mutable row = c.Up
    while row <> (c :> DlxNode) do
        let mutable left = row.Left
        while left <> row do
            left.Column.Size <- left.Column.Size + 1
            left.Down.Up <- left
            left.Up.Down <- left
            left <- left.Left
        row <- row.Up
    c.Right.Left <- c
    c.Left.Right <- c

let chooseColumn () =
    let mutable best: DlxColumn = null
    let mutable minSize = Int32.MaxValue
    let mutable curr = root.Right
    while curr <> (root :> DlxNode) do
        let col = curr :?> DlxColumn
        if col.Size < minSize then
            minSize <- col.Size
            best <- col
        curr <- curr.Right
    best

let rec dlxSearch k (solution: int[]) =
    dlxIterations <- dlxIterations + 1
    
    if root.Right = (root :> DlxNode) then true // solved
    else
        let col = chooseColumn()
        if col.Size = 0 then false
        else
            cover col
            let mutable row = col.Down
            let mutable found = false
            
            while row <> (col :> DlxNode) && not found do
                solution.[k] <- row.RowId
                let mutable right = row.Right
                while right <> row do
                    cover right.Column
                    right <- right.Right
                
                if dlxSearch (k + 1) solution then
                    found <- true
                else
                    // Backtrack
                    let mutable left = row.Left
                    while left <> row do
                        uncover left.Column
                        left <- left.Left
                    solution.[k] <- -1
                
                row <- row.Down
                
            uncover col
            found

let coverClues () =
    for r in 0 .. 8 do
        for c in 0 .. 8 do
            if puzzle.[r,c] <> 0 then
                let n = puzzle.[r,c]
                // Find matching rowId
                let mutable found = false
                let mutable id = 0
                while not found && id < 729 do
                   if rowStarts.[id] <> null && rowInfo.[id].Row = r && rowInfo.[id].Col = c && rowInfo.[id].Num = n then
                       found <- true
                       let mutable curr = rowStarts.[id]
                       let start = curr
                       let mutable first = true
                       while first || curr <> start do
                           first <- false
                           cover curr.Column
                           curr <- curr.Right
                   id <- id + 1

let extractSolution (sol: int[]) =
    solutionGrid <- Array2D.copy puzzle
    for i in 0 .. 80 do
        let id = sol.[i]
        if id >= 0 then
            let info = rowInfo.[id]
            solutionGrid.[info.Row, info.Col] <- info.Num

let printGrid (grid: int[,]) =
    for r in 0 .. 8 do
        for c in 0 .. 8 do
            printf "%d " grid.[r,c]
        printfn ""

let readMatrix (file: string) =
    if not (File.Exists file) then
        eprintfn "Error reading %s" file
        false
    else
        let display = if file.StartsWith("/app/Matrices/") then "../" + file.Substring(5) else file
        printfn "%s" display
        let lines = File.ReadAllLines file |> Array.filter (fun l -> not (String.IsNullOrWhiteSpace l) && not (l.StartsWith "#"))
        let mutable r = 0
        for l in lines do
            if r < 9 then
                let parts = l.Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
                if parts.Length = 9 then
                    for c in 0 .. 8 do
                        puzzle.[r,c] <- int parts.[c]
                        printf "%d " puzzle.[r,c]
                    printfn ""
                    r <- r + 1
        r = 9

[<EntryPoint>]
let main args =
    let sw = Stopwatch.StartNew()
    
    for arg in args do
        if readMatrix arg then
            printfn "\nPuzzle:"
            printGrid puzzle
            
            dlxIterations <- 0
            initDlxMatrix()
            buildMatrix()
            coverClues()
            
            let solution = Array.create 81 (-1)
            let result = dlxSearch 0 solution
            
            if result then
                extractSolution solution
                printfn "\nPuzzle:"
                printGrid solutionGrid
                printfn "\nSolved in Iterations=%d\n" dlxIterations
            else
                printfn "\nNo solution found"
    
    sw.Stop()
    printfn "Seconds to process %.3f" sw.Elapsed.TotalSeconds
    0
