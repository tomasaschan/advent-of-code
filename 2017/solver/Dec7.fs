module TLycken.AdventOfCode.Solutions.Dec7

open TLycken.AdventOfCode.Utils
open TLycken.AdventOfCode.Utils.Debugging

type Reference = { name : string }
type Program = { name : string; weight : int }
type Atlas = { atlas : Reference; world : Reference list }

module Parse =
  type AtlasLine = { carrier : Program; world : Reference list }
  type Line = PL of Program | AL of AtlasLine

  let line = function
    | Regex.Match "^([a-z]+) \((\d+)\) -> (?:([a-z]+)(?:, )?)+$" names ->
      match names with
      | atlas :: (w :: world) ->
        match Parse.int w with
        | Some weight ->
          {
            carrier = { name = atlas; weight = weight }
            world = world |> List.map (fun n -> { name = n })
          }
          |> AL
          |> Some
        | _ ->
          None
      | _ -> None
    | Regex.Match "^(\w+) \((\d+)\)$" [program; w] ->
      match Parse.int w with
      | Some weight ->
        {
          name = program
          weight = weight
        }
        |> PL |> Some
      | _ ->
        printfn "failed to parse int from %s (%A)" w [program;w]
        None
    | _ -> None

  let lines = List.choose line

  let interpretProgram = function
    | AL { carrier = p } -> p
    | PL p -> p

  let interpretAtlas = function
    | AL { carrier = { name = n }; world = world } ->
      {
        atlas = { name = n };
        world = world
      } |> Some
    | _ -> None

  let weights =
    lines
    >> List.map (interpretProgram >> (fun { name = n; weight = w } -> n,w))
    >> Map.ofList
  let atlases =
    lines
    >> List.choose interpretAtlas
    >> List.map (fun { atlas = { name = n }; world = w } -> n,w |> List.map (fun { name = n } -> n))
    >> Map.ofList


let root input =
  let programs = input |> Parse.atlases |> Map.toList
  let carriers = programs |> List.map fst |> Set.ofList
  let carriees = programs |> List.collect snd |> Set.ofList

  Set.difference carriers carriees |> Set.maxElement

module A =

  let solve = root

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
    let root = root input
    let weights = input |> Parse.weights
    let discs = input |> Parse.atlases

    // let total = identifyMismatch weights discs 0 root

    // sprintf "%i" total
    "TODO"

let solvers = A.solve, B.solve