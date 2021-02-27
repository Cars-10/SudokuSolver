open System
open System.IO

let puzzle = Array2D.init 9 9 (fun x y -> 0)
//printfn "%A" puzzle

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
    printfn "%s" filename
    let lines = System.IO.File.ReadAllLines(filename)
    let mutable j = 0
    for line in lines do
        if not(line.StartsWith("#")) then
            let mutable i = 0
            for value in line.Split(" ") do
                //printfn "j,i: %i,%i" j i
                puzzle.[j,i] <- value |> int
                i <- i + 1
            j <- j + 1    

[<EntryPoint>]
let main args =
    //for item in Environment.GetCommandLineArgs() do
    for item in Environment.GetCommandLineArgs()  do
        //printfn "%s" item
        if item.EndsWith(".matrix") then
            readMatrixFile item
            printfn "hello"
            printPuzzle
