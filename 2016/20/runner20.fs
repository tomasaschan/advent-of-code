open AoC.Utils.Runner
open AoC.Utils.Helpers
open AoC.Dec20.Solver

[<EntryPoint>]
let main argv = 

    let lo, hi, args =
        match List.ofArray argv with
        | l::h::a ->
            Int.parseBig l, Int.parseBig h, a
        | _ -> Some (bigint 0), Some (bigint System.UInt32.MaxValue), List.ofArray argv

    match lo, hi, args with
    | Some l, Some h, _ -> solveFromFile (A.solve l h) (B.solve l h) (Array.ofList args)
    | _ -> printfn "couldn't parse input"

    0
