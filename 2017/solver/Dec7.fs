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

  type Program = {
    name : string
    weight : int
    children : Program list
  }

  let solve input =
    let weights, discs = Parse.lines input

    let rec buildTree root = 
      let weight n = Map.find n weights
      let children name = Map.tryFind name discs |> orDefault List.empty

      { name = root; weight = weight root; children = children root |> List.map buildTree }

    let rec totalWeight p =
      p.weight + (p.children |> List.sumBy totalWeight)

    let isBalanced program = 
      program.children
      |> List.map totalWeight
      |> Set.ofList
      |> Set.count
      |> (=) 1

    let rec findCorrection root =
      if isBalanced root then
        None
      elif root.children |> List.forall isBalanced then
        let childWeights = root.children |> List.map totalWeight
        let partitioner w = childWeights |> List.filter ((=) w) |> List.length |> (=) 1
        let bad, good = childWeights |> List.partition partitioner
        let goodW, badW = List.head good, List.head bad

        let badProgram = root.children |> List.find (fun p -> totalWeight p = badW)

        Some (goodW - (badProgram.children |> List.sumBy totalWeight))
      else
        root.children
        |> List.choose findCorrection
        |> List.tryHead

    root discs
    |> buildTree
    |> findCorrection
    |> Option.get
    |> sprintf "%i"

let solvers = A.solve, B.solve
