// Learn more about F# at http://fsharp.org

open System
open AoC.Utils.Runner
open AoC.Dec16.Solver

[<EntryPoint>]
let main argv = 

    solve1 argv A.solve B.solve |> printfn "%s"

    0 // return an integer exit code
