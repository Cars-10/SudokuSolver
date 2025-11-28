open System
open System.IO

let mutable puzzle = Array2D.init 9 9 (fun x y -> 0)
let mutable count = 0

let printPuzzle () = 
    printfn "\nPuzzle:"
    for j in 0 .. 8 do
        for i in 0 .. 8 do
            printf "%i " puzzle.[j,i]
        printfn ""
    

let readMatrixFile (filename : String) =
    if not(File.Exists (filename))  then
        printfn "File does not exist: %s" filename 
        exit 0
    //printfn "%s" filename
    let lines = System.IO.File.ReadAllLines(filename)
    let mutable j = 0
    for line in lines do
        if not(line.StartsWith("#")) then
            let mutable i = 0
            for value in line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) do
                if j < 9 && i < 9 then
                    puzzle.[j,i] <- value |> int
                i <- i + 1
            if i > 0 then j <- j + 1    

let isPossible y x pval =
    let mutable possible = true
    for i in 0 .. 8 do
        if puzzle.[i,x] = pval then possible <- false
    for i in 0 .. 8 do
        if puzzle.[y,i] = pval then possible <- false
    
    let x0 = (x/3)*3
    let y0 = (y/3)*3
    for i in 0 .. 2 do
        for j in 0 .. 2 do
            if puzzle.[y0+i,x0+j] = pval then possible <- false
    possible

let rec solve () =
    let mutable solved = false
    let mutable foundEmpty = false
    let mutable j = 0
    let mutable i = 0
    
    // Find first empty cell
    let mutable breakLoop = false
    let mutable r = 0
    let mutable c = 0
    while r < 9 && not breakLoop do
        c <- 0
        while c < 9 && not breakLoop do
            if puzzle.[r,c] = 0 then
                j <- r
                i <- c
                foundEmpty <- true
                breakLoop <- true
            c <- c + 1
        r <- r + 1

    if not foundEmpty then
        printPuzzle()
        printfn "Solved in Iterations=%i\n" count
        2 // Success
    else
        let mutable result = 0
        for pval in 1 .. 9 do
            count <- count + 1
            if result <> 2 then
                if isPossible j i pval then
                    puzzle.[j,i] <- pval
                    if solve() = 2 then
                        result <- 2
                    else
                        puzzle.[j,i] <- 0
        result

[<EntryPoint>]
let main args =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
    for item in args do
        printfn "\n%s" item
        if item.EndsWith(".matrix") then
            readMatrixFile item
            printPuzzle()
            count <- 0
            let _ = solve()
            ()
    stopwatch.Stop()
    printfn "Seconds to process %.3f" (stopwatch.Elapsed.TotalSeconds)
    0
