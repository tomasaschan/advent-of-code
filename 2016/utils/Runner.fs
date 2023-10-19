namespace AoC.Utils

module Reader =

    let read filename =
        if System.IO.File.Exists filename then System.IO.File.ReadLines filename else Seq.empty<string>
        |> List.ofSeq

    let fromFile =
        function
        | a :: [ b ] -> Some(read a, read b)
        | [ both ] -> Some(read both, read both)
        | _ -> None

    let fromArgs =
        function
        | a :: [ b ] -> Some(a, b)
        | [ both ] -> Some(both, both)
        | _ -> None

module Printer =

    let private output1 problem (solution, time) =
        sprintf "%s: %s (%d ms)" problem solution time

    let private output a b =
        sprintf "%s\n%s" (output1 "a" a) (output1 "b" b)

    let sprint =
        function
        | Some (a, b) -> output a b
        | _ -> "(no solutions)"

module Runner =

    open Reader
    open Printer

    let private time fn =
        let watch = System.Diagnostics.Stopwatch.StartNew()
        let answer = fn ()
        watch.Stop()

        answer, watch.ElapsedMilliseconds

    let private lift solver input = fun () -> solver input

    let private solvePart solution input = lift solution input |> time

    let private solveBoth sola solb =
        function
        | Some (ina, inb) -> Some(solvePart sola ina, solvePart solb inb)
        | None -> None


    let private solve a b input =
        printfn "Solving..."
        solveBoth a b input |> sprint |> printfn "%s"

    let solveFromFile a b = List.ofArray >> fromFile >> solve a b

    let solveFromArgs a b = List.ofArray >> fromArgs >> solve a b

    let todo _ = "not implemented"
