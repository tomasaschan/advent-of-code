// Learn more about F# at http://fsharp.org

open System
open AoC.Dec10.Solver
open AoC.Utils.Runner

[<EntryPoint>]
let main argv = 

    solve argv A.solve B.solve |> printfn "%s"

    0
