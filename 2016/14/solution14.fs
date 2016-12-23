namespace AoC.Dec14

module Domain =

    open AoC.Utils

    let fiver (s,i) =
        match s with
        | Regex.Match "(.)\1{4}" [c] -> Some (c,i)
        | _ -> None

    let three c (s,i) =
        match s with
        | Regex.Match "(.)\1{2}" [c'] when c = c' -> Some i
        | _ -> None

    let positive i = i >= 0
    let prequels i = Seq.init 1000 (fun i' -> i - 1000 +  i') |> Seq.filter positive
    let hashed hasher input = Seq.map (fun i -> hasher input i, i)
    let isValidKey hasher input c = prequels >> hashed hasher input >> Seq.choose (three c)
    let valid hasher input (c,i) = isValidKey hasher input c i
    let keys n hasher input =
        Seq.initInfinite id
        |> hashed hasher input
        |> Seq.choose fiver
        |> Seq.collect (valid hasher input)
        |> Seq.distinct
        |> Seq.take (2 * n)
        |> Seq.sort
        |> Seq.take n

    let hash = Hash.md5 >> String.lowercase

    let findKey hasher = keys 64 hasher >> Seq.last

module Solver =

    open Domain

    module A =

        let once input = sprintf "%s%d" input >> hash

        let solve = findKey once >> sprintf "%d"

    module B =

        let rec repeated hasher i str =
            if i = 0 then str
            else repeated hasher (i-1) (hasher str)

        let stretched input = sprintf "%s%d" input >> hash >> repeated hash 2016

        let solve = findKey stretched >> sprintf "%d"
