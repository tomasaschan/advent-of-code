namespace AoC.Dec6

module Domain =

    open AoC.Utils.Helpers

    let classify str =
        str
        |> asChars
        |> List.mapi (fun i c -> i,c)

    let pos = fst
    let char = snd

    let mostPopular = (List.groupBy id >> List.maxBy (snd >> List.length) >> fst)

    let leastPopular = (List.groupBy id >> List.minBy (snd >> List.length) >> fst)

    let chars = snd >> List.map char

module Solver =

    module A =

        open AoC.Utils.Helpers
        open Domain

        let solve = List.collect classify >> List.groupBy pos >> List.map (chars >> mostPopular) >> join

    module B =

        open AoC.Utils.Helpers
        open Domain

        let solve = List.collect classify >> List.groupBy pos >> List.map (chars >> leastPopular) >> join
