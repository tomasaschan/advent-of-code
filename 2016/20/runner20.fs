// Learn more about F# at http://fsharp.org

open System
open AoC.Utils.Runner
open AoC.Utils.Helpers
open AoC.Dec20.Solver

[<EntryPoint>]
let main argv = 

    let lo, hi, args =
        match List.ofArray argv with
        | l::h::a ->
            printfn "forwarding args %A" a
            Int.parseBig l, Int.parseBig h, a
        | _ -> Some (bigint 0), Some (bigint System.UInt32.MaxValue), List.ofArray argv

    match lo, hi, args with
    | Some l, Some h, _ -> solve (Array.ofList args) (A.solve l h) (B.solve l h)
    | _ -> "couldn't parse input"
    |> printfn "%s"

    0 // return an integer exit code
