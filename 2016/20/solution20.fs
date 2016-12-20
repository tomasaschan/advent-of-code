namespace AoC.Dec20

module Domain =

    let one = bigint 1

    let rec validFrom lo hi blocked =
        match blocked with
        | [] ->
            printfn "no more blocked addresses; returning %A-%A" lo hi
            [lo, hi]
        | b::bs ->
            if lo < fst b
            then
                printfn "there are some unblocked addresses here! %A-%A" lo (fst b - one)
                (lo, fst b - one) :: (validFrom (snd b + one) hi bs)
            else
                printfn "%A is in a blocked range (%A-%A) - looking from %A..." lo (fst b) (snd b) (snd b + one)
                validFrom (snd b + one) hi (bs |> List.skipWhile (fun r -> snd r < snd b + one))

    let validRanges lo hi = List.sort >> (validFrom lo hi)

module Parse =

    open AoC.Utils.Helpers

    let private pint = Int.parseBig

    let private ints = function
        | Regex.Match "(\d+)-(\d+)" [a;b] -> pint a, pint b
        | _ -> (None, None)

    let ipRanges<'a> = List.map ints >> List.choose (function
        | (Some a, Some b) -> Some (a, b)
        | _ -> None)
    

module Solver =

    open Parse
    open Domain

    module A =

        let firstValid lo hi = validRanges lo hi >> Seq.head >> fst

        let solve<'a> lo hi = Parse.ipRanges >> firstValid lo hi >> sprintf "%A"

    module B =

        let addressesInRange range = snd range - fst range + one

        let validAddressCount lo hi = validRanges lo hi >> Seq.sumBy addressesInRange

        let solve<'a> lo hi = Parse.ipRanges >> validAddressCount lo hi >> sprintf "%A"
