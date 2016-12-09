open AoC.Utils.Runner
open AoC.Dec8.Solver

[<EntryPoint>]
let main argv = 

    solve argv A.solve todo |> printfn "%s"

    let apply f (a,b) = f a b

    0
