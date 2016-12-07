// Learn more about F# at http://fsharp.org

open AoC.Utils.Runner
open AoC.Dec7.Solvers

[<EntryPoint>]
let main argv = 

    solve argv A.solve B.solve |> printfn "%s"

    0 // return an integer exit code
