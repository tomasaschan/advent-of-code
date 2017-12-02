open TLycken.AdventOfCode
open TLycken.AdventOfCode.Solutions

let printUsage () =
    printfn "Usage: aoc <day> <input-file>"
    printfn "\twhere <day> is the day which to solve, and <input-file> is the path to an existing file"

let solvers =
    Map.empty
    |> Map.add 1 Dec1.solvers
    |> Map.add 2 Dec2.solvers

[<EntryPoint>]
let main argv =
    
    Runner.run solvers printUsage argv
