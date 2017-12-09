module TLycken.AdventOfCode.Solutions.Dec7

open TLycken.AdventOfCode.Utils

module Parse =
  let line weights discs = function
    | Regex.Match "^([a-z]+) \((\d+)\) -> (?:([a-z]+)(?:, )?)+$" matches ->
      match matches with
      | carrier :: (weight :: above) ->
        match Parse.int weight with
        | Some w -> Map.add carrier w weights, Map.add carrier above discs
        | _ -> weights, discs
      | _ -> weights, discs
    | Regex.Match "^(\w+) \((\d+)\)$" matches ->
      match matches with
      | [program; weight] ->
        match Parse.int weight with
        | Some w -> Map.add program w weights, discs
        | _ -> weights, discs
      | _ -> weights, discs
    | _ -> weights, discs

  let lines = List.fold (fun (weights, discs) l -> line weights discs l) (Map.empty, Map.empty)

let carrier discs name =
  let keyFinder _ ps = ps |> List.contains name
  Map.tryFindKey keyFinder discs


let rec _root discs candidate =
  match carrier discs candidate with
  | Some c -> _root discs c
  | None -> candidate

let root discs =
  let firstCandidate = discs |> Map.toList |> List.head |> fst
  _root discs firstCandidate

let onTopOf discs name = Map.tryFind name discs |> orDefault []

module A =

  let solve = Parse.lines >> snd >> root

module B =

  let balanced : int list -> bool = List.distinct >> List.length >> (=) 1

  let minmax lst = List.min lst, List.max lst

  let outlier = List.groupBy id >> List.sortBy (snd >> List.length) >> List.head >> snd >> List.head

  let solve input =
    let weights, discs = Parse.lines input

    let rec totalWeight name =
      let self = Map.find name weights
      let above = onTopOf discs name |> List.sumBy totalWeight
      self + above

    let rec findImbalancedTower candidate =
      let above = Map.find candidate discs
      let was = above |> List.map totalWeight

      if balanced was
      then candidate
      else
        let i = List.findIndex (fun w -> w = outlier was) was
        let candidate' = above.[i]
        findImbalancedTower candidate'

    let root = root discs
    let imbalanced = findImbalancedTower root
    let parent = match carrier discs imbalanced with Some p -> p | None -> failwith "fuck"
    let above = Map.find parent discs |> List.map totalWeight

    let strange = outlier above

    let i = List.findIndex ((=) strange) above
    let outlier = above.[i]
    let j = List.findIndex ((<>) strange) above
    let normal = above.[j]
    let im = Map.find imbalanced weights
    im + normal - outlier |> sprintf "%i"

let solvers = A.solve, B.solve
