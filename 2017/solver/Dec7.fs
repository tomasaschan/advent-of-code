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

let root weights discs =
  let carriers = weights |> Map.toList |> List.map fst |> Set.ofList
  let carriees = discs |> Map.toList |> List.collect snd |> Set.ofList

  Set.difference carriers carriees |> Set.maxElement

module A =

  let solve = Parse.lines >> (fun (ws,ds) -> root ws ds)

module B =

  let balanced towers = List.distinct towers |> List.length = 1

  let rec calculate weights discs name =
    let self = Map.find weights name

    match Map.tryFind name discs with
    | Some towers ->
      let above = towers |> List.map (calculate weights discs)
      if not << 
      self, List.sum above, above |> List.map snd |> balanced
    | None -> self, self, true

  // let rec aboveSum weights discs name =
    

  let solve input =

    // let total = identifyMismatch weights discs 0 root

    // sprintf "%i" total
    "TODO"

let solvers = A.solve, B.solve
