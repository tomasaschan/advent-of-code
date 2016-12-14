// Learn more about F# at http://fsharp.org

open System
open AoC.Dec12.Solver

[<EntryPoint>]
let main argv =

    AoC.Utils.Runner.solve argv A.solve B.solve |> printfn "%s"

    0 // return an integer exit code
