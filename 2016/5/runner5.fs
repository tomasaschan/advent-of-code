// Learn more about F# at http://fsharp.org

open AoC.Utils.Runner
open AoC.Dec5.Solver

[<EntryPoint>]
let main argv = 

    let solution = solve1 argv A.solve todo

    printfn "%s" solution

    0 // return an integer exit code
