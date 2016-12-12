// Learn more about F# at http://fsharp.org

open System
open AoC.Utils.Runner
open AoC.Dec9.Solver

[<EntryPoint>]
let main argv = 

    printfn "%s" (solve argv A.solve B.solve)

    0 // return an integer exit code
