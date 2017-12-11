open TLycken.AdventOfCode
open TLycken.AdventOfCode.Solutions

let printUsage () =
    printfn "Usage: dotnet run [<day>]"
    printfn "\twhere <day> is the day which to solve, and the input for the problem is at .\\input\\dec<day>.txt"
    printfn "\tIf no <day> is given, all implemented solutions are run."

let solvers =
    [
        Dec1.solvers
        Dec2.solvers
        Dec3.solvers
        Dec4.solvers
        Dec5.solvers
        Dec6.solvers
        Dec7.solvers
        Dec8.solvers
        Dec9.solvers
        Dec10.solvers
        Dec11.solvers
    ] |> List.mapi (fun i s -> i+1,s) |> Map.ofList

[<EntryPoint>]
let main argv =

    Runner.run solvers printUsage argv
