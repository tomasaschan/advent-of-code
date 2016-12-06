// Learn more about F# at http://fsharp.org

open AoC.Utils.Runner
open AoC.Dec6.Solver

[<EntryPoint>]
let main argv = 

    let solution = solve argv A.solve B.solve

    printfn "%s" solution

    0 // return an integer exit code
