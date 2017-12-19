namespace TLycken.AdventOfCode.Utils

open FSharp.Core

module List =

  let insert i x list =
    let before, after = list |> List.indexed |> List.partition (fst >> fun i' -> i' <= i)

    let before', after' = List.map snd before, List.map snd after

    List.append before' (x :: after')
