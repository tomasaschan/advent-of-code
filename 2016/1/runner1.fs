open AoC.Utils.Runner
open AoC.Dec1.Solutions

[<EntryPoint>]
let main argv =

    solveFromFile A.solve B.solve  argv
    
    0
