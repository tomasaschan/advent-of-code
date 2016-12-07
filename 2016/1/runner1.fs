// Learn more about F# at http://fsharp.org

open AoC.Utils.Runner
open AoC.Dec1.Solutions

[<EntryPoint>]
let main argv =

    let solutions = solve1 argv A.solve B.solve
    
    printfn "%s" solutions
    0 // return an integer exit code
