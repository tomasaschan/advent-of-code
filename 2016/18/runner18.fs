// Learn more about F# at http://fsharp.org

open System
open AoC.Utils.Runner
open AoC.Dec18.Solver
open AoC.Utils.Helpers

[<EntryPoint>]
let main argv = 

    let count, input =
        match List.ofArray argv with
        | c::i -> Int.parse c, Array.ofList i
        | _ -> Some 3, [|"short-sample.txt"|]
    
    match count with
        | Some c -> solveFromFile (A.solve c) todo input
        | None -> printfn "incorrect input"
    
    0
