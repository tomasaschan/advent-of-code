open TLycken.AdventOfCode
open TLycken.AdventOfCode.Solutions

let printUsage () =
    printfn "Usage: dotnet run [<day>]"
    printfn "\twhere <day> is the day which to solve, and the input for the problem is at .\\input\\dec<day>.txt"
    printfn "\tIf no <day> is given, all implemented solutions are run."

let solvers =
    Map.empty
    |> Map.add 1 Dec1.solvers
    |> Map.add 2 Dec2.solvers
    |> Map.add 3 Dec3.solvers
    |> Map.add 4 Dec4.solvers
    |> Map.add 5 Dec5.solvers
    |> Map.add 6 Dec6.solvers

[<EntryPoint>]
let main argv =

    Runner.run solvers printUsage argv
