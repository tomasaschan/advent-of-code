// Learn more about F# at http://fsharp.org

open AoC.Utils.Runner
open Dec1

[<EntryPoint>]
let main argv =

    let solutions = solve argv A.solve todo

    printfn "%s" solutions
    0 // return an integer exit code
