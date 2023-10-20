module TLycken.AdventOfCode.Solutions.Dec3

open TLycken.AdventOfCode.Utils

let parse = Parse.int >> function Some i -> i | _ -> failwith "Invalid input, expected an integer"

let roundUpToOdd i =
  match i % 2, i with
  | 1, i' -> i'
  | 0, i' -> i' + 1
  | _ -> failwith (sprintf "Math is broken; %i %% 2 is neither 0 nor 1" i)
let side = (float >> sqrt >> ceil >> int >> roundUpToOdd)
let inside side = pown (side - 2) 2
let lastrim count =
  let s = side count
  let i = inside s
  (count - i - 1) % (s - 1) + 1

module A =
  let manhattan input =
    let s = side input
    let inward = (s - 1) / 2
    let angular = abs (lastrim input - inward)
    inward + angular
  let solve = List.head >> parse >> manhattan >> sprintf "%A"

module B =
  open Debugging

  let get grid pos = (Map.tryFind pos >> orDefault 0) grid
  let put pos = Map.add pos

  let next x y grid =
    let up,down,left,right = (x,y+1),(x,y-1),(x-1,y),(x+1,y)
    let isFilled = get grid >> fun x -> x <> 0
    
    match isFilled up, isFilled down, isFilled left, isFilled right with
    // along edges
    | false, true, true, false -> up
    | false, true, false, true -> left
    | true, false, false, true -> down
    | true, false, true, false -> right
    // when it's time to turn
    | false, true, false, false -> left
    | false, false, false, true -> down
    | true, false, false, false -> right
    | false, false, true, false -> up
    // the very first move
    | false, false, false, false -> right
    | _ -> failwithf "Could not handle being surrounded by %A" (up,down,left,right)

  let adjacents (x, y) = [
    x + 1, y
    x + 1, y + 1
    x, y + 1
    x - 1, y + 1
    x - 1, y
    x - 1, y - 1
    x, y - 1
    x + 1, y - 1
  ]

  let getValue grid x y = next x y grid |> adjacents |> List.sumBy (get grid)


  let rec buildMemory valueGetter target grid x y =
    let (x',y') = next x y grid
    let value = valueGetter grid x y
    if value >= target
    then (x',y'), value
    else buildMemory valueGetter target (grid |> put (x',y') value) x' y'

  let firstLargerThan input =
    let grid = Map.empty |> Map.add (0,0) 1
    buildMemory getValue input grid 0 0
    |> snd

  let solve = List.head >> parse >> firstLargerThan >> sprintf "%i"

let solvers = A.solve, B.solve