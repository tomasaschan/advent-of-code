// Learn more about F# at http://fsharp.org

open System
open AoC.Utils.Runner
open AoC.Dec12.Solver

[<EntryPoint>]
let main argv =

    solve argv A.solve todo |> printfn "%s"

    0 // return an integer exit code
