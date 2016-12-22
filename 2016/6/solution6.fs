namespace AoC.Dec6

module Domain =

    open AoC.Utils

    let classify str =
        str
        |> String.asChars
        |> List.mapi (fun i c -> i,c)

    let pos = fst
    let char = snd

    let mostPopular = (List.groupBy id >> List.maxBy (snd >> List.length) >> fst)

    let leastPopular = (List.groupBy id >> List.minBy (snd >> List.length) >> fst)

    let chars = snd >> List.map char

    let find sorter = List.collect classify >> List.groupBy pos >> List.map (chars >> sorter) >> String.join

module Solver =

    module A =

        open AoC.Utils
        open Domain

        let solve = find mostPopular

    module B =

        open AoC.Utils
        open Domain

        let solve = find leastPopular
