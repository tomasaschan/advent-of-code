// Learn more about F# at http://fsharp.org

open System
open AoC.Utils
open AoC.Dec15.Solver
open AoC.Dec15.Theory

[<EntryPoint>]
let main argv =

    Runner.solve argv A.solve B.solve |> printfn "%s"

    0 // return an integer exit code
