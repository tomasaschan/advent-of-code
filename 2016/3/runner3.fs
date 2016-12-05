open AoC.Utils.Runner

open AoC.Dec3.Solvers

[<EntryPoint>]
let main argv = 
    
    let solution = solve argv A.solve B.solve

    printfn "%s" solution

    0 // return an integer exit code
