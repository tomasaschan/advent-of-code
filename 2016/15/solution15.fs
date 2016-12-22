namespace AoC.Dec15

module Domain =

    type Disc = { index : int ; slots : int; position : int }

    let disc index slots position = { index = index ; slots = slots ; position = position }

    let index d = d.index
    let slots d = d.slots
    let position d = d.position

    let isValid t = List.forall (fun d -> ((position d) + (t + d.index)) % (slots d) = 0)

    let releaseTime discs =
        Seq.initInfinite (fun i -> i + 1)
        |> Seq.filter (fun t -> isValid t discs)
        |> Seq.head

module Parse =

    open AoC.Utils
    open Domain

    let private parts = function
        | Regex.Match "^Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+).$" [i; tot; now] -> Some (List.map Int.parse [i; tot; now])
        | _ -> None

    let private _disc str =
        match parts str with
        | Some [Some i; Some tot; Some now] -> Some (disc i tot now)
        | _ -> None

    let discs = List.choose _disc

module Solver =
    open Domain

    module A =

       let solve = Parse.discs >> releaseTime >> sprintf "%d"

    module B =

        let solve input =
            let discs = Parse.discs input

            let extra = { index = List.length discs + 1 ; slots = 11 ; position = 0 }

            let t = releaseTime (List.append discs [extra])

            sprintf "%d" t
