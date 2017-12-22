module TLycken.AdventOfCode.Solutions.Dec21

open TLycken.AdventOfCode.Utils

module Fractal =
  let init =
    [[false; true ; false]
     [false; false; true ]
     [true ; true ; true ]]

  let flipH = List.map List.rev
  let flipV = List.rev >> List.map (List.map id)

  let private index<'a> = List.indexed >> List.collect (fun (i, row) -> row |> List.indexed |> List.map (fun (j,x) -> (i,j), x))

  let rot<'a> =
    index
    >> List.groupBy (fst >> snd)
    >> List.map (snd >> (List.sortByDescending (fst >> fst)) >> List.map snd)

  type Permutation = FlipH | FlipW | Rotate
  let allPermutations = seq {
    yield Rotate
    yield Rotate
    yield Rotate
    yield Rotate
    yield FlipH
    yield Rotate
    yield Rotate
    yield Rotate
  }

  let apply image = function
    | FlipH -> flipH image
    | FlipW -> flipV image
    | Rotate -> rot image

  let permute image = allPermutations |> Seq.scan apply image |> Seq.skip 1

  let private indexL<'a> = List.indexed >> List.collect (fun (i, row) -> row |> List.indexed |> List.map (fun (j,x) -> (i,j), x))
  let rotL<'a> =
    indexL
    >> List.groupBy (fst >> snd)
    >> List.map (snd >> (List.sortByDescending (fst >> fst)) >> List.map snd)

  let split side =
    List.chunkBySize side
    >>
    List.map (List.map (List.chunkBySize side) >> rotL >> List.map List.rev)
    >>
    List.collect id

  let unsplit n =
    List.chunkBySize n
    >> List.map (
      List.collect (List.indexed)
      >>
      List.groupBy fst
      >>
      List.map (snd >> List.collect snd)
    )
    >> List.collect id

  let print = List.map (List.map (function true -> "#" | false -> ".") >> String.concat "") >> String.concat "\n"

  let measure = List.collect id >> List.filter id >> List.length

module Parse =
  let row = String.asChars >> List.choose (function '.' -> Some false | '#' -> Some true | _ -> None)
  let pattern = String.split "/" >> List.map row

  let rule = String.split " => " >> function [a; b] -> Some (pattern a, pattern b) | _ -> None

  let rules = List.choose rule >> Map.ofList

module Evolution =
  let evolveSquare rules = Fractal.permute >> Seq.pick (fun s -> Map.tryFind s rules)
  let evolveOneStep (square : bool list list) rules =
    let size = List.length square
    if size = 2 || size = 3 then
      evolveSquare rules square
    elif size % 2 = 0 then
      let squares = Fractal.split 2 square
      let squares' = List.map (evolveSquare rules) squares
      let square' = Fractal.unsplit (size / 2) squares'
      square'
    elif size % 3 = 0 then
      let squares = Fractal.split 3 square
      let squares' = List.map (evolveSquare rules) squares
      let square' = Fractal.unsplit (size / 3) squares'
      square'
    else
      failwithf "Size %i is not 2, 3 or divisible by 2 or 3" size

  let rec evolve steps state rules =
    if steps = 0
    then Fractal.measure state
    else
      let state' = evolveOneStep state rules
      evolve (steps-1) state' rules 

let solve steps = Parse.rules >> Evolution.evolve steps Fractal.init >> sprintf "%i"

let solvers = solve 5, solve 18