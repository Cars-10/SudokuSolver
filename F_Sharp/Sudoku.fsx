open System
open System.IO

let puzzle = Array2D.init 9 9 (fun x y -> 0)
//printfn "%A" puzzle

let readMatrixFile (filename : String) =
    printfn "%s" filename
    if not(File.Exists (filename))  then
        printfn "File does not exist: %s" filename 
        exit 0
    let lines = System.IO.File.ReadAllLines(filename)
    let i = 0
    for line in lines do
        if not(line.StartsWith("#")) then
            let j = 0
            for value in line.Split(" ") do
                puzzle.[i,j] <- int.Parse(value)
                j++
        i++
    
    return 0


// printfn "env.cmdline: %A" <| Environment.GetCommandLineArgs()    
for item in Environment.GetCommandLineArgs() do
  
  if item.EndsWith(".matrix") then
    // printfn "%s" item
    readMatrixFile item