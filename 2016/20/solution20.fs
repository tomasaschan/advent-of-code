namespace AoC.Dec20

module Domain =

    let one = bigint 1

    let rec validFrom lo hi blocked =
        match blocked with
        | [] -> [lo, hi]
        | b::bs ->
            if lo < fst b
            then (lo, (fst b - one)) :: (validFrom (snd b + one) hi bs)
            else
                if fst b <= lo && lo <= snd b
                then validFrom (snd b + one) hi bs
                else validFrom lo hi bs
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
