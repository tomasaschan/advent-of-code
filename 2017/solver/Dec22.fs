module TLycken.AdventOfCode.Solutions.Dec22

open TLycken.AdventOfCode.Utils

type State = Clean | Weakened | Infected | Flagged

type Grid = System.Collections.Generic.Dictionary<int*int,State>
module Grid =
  let empty = Grid()

  let infect pos (grid : Grid) =
    grid.[pos] <- Infected
  let clean pos (grid : Grid) =
    grid.[pos] <- Clean
  let weaken pos (grid : Grid) =
    grid.[pos] <- Weakened
  let flag pos (grid : Grid) = 
    grid.[pos] <- Flagged
  let stateOf pos  (grid : Grid) =
    if grid.ContainsKey pos
    then grid.[pos]
    else Clean

  let print h w x0 y0 pos grid =
    let wrap p c = if p = pos then ("[" + c + "]") else " " + c + " "
    let sq p =
      stateOf p grid
      |>
      function
      | Clean -> "."
      | Weakened -> "W"
      | Infected -> "#"
      | Flagged -> "F"

    let row y =
      List.init w ((+) x0)
      |> List.map (fun x -> sq (x,y) |> wrap (x,y))
      |> String.concat ""
    List.init h ((+) y0)
    |> List.map row
    |> String.concat "\n"
    |> printfn "%s"
    printfn "\n"

module Parse =
  let cell = function
    | '#' -> Some Infected
    | '.' -> Some Clean
    | _ -> None

  let grid input =
    let positions = List.map (String.asChars >> List.choose cell >> List.indexed) >> List.indexed >> List.collect (fun (y, xvs) -> List.map (fun (x, v) -> (x,y),v) xvs)
    let height = List.length input
    let width = input |> List.head |> String.length
    let middle x = float x |> (fun x -> x/2.) |> floor |> int
    let map = Grid()
    input |> positions |> List.iter (fun (pos, state) -> map.[pos] <- state)
    map, (middle width, middle height)

module Virus =

  type Direction = Up | Down | Left | Right
  type Turn = Cw | Ccw | Front | Reverse

  let step (x,y) = function
    | Up -> (x, y-1)
    | Down -> (x, y+1)
    | Left -> (x-1, y)
    | Right -> (x+1, y)

  let turn dir t =
    match dir, t with
    | Up, Cw | Down, Ccw | Left, Reverse | Right, Front-> Right
    | Down, Cw | Up, Ccw | Right, Reverse | Left, Front-> Left
    | Left, Cw | Right, Ccw | Down, Reverse | Up, Front-> Up
    | Right, Cw | Left, Ccw | Up, Reverse | Down, Front-> Down

let rec walk turnDir flip steps count pos dir grid =
  if steps = 0
  then count
  else
    let state = Grid.stateOf pos grid
    let dir' = Virus.turn dir (turnDir state)
    let pos' = Virus.step pos dir'
    let action, increment = flip pos grid
    action pos grid
    let count' = count + increment
    let steps' = steps - 1
    walk turnDir flip steps' count' pos' dir' grid

module A =
  open Virus
  let flip pos = 
    Grid.stateOf pos
    >>
    function
    | Infected -> Grid.clean, 0
    | Flagged -> Grid.clean, 0
    | Weakened -> Grid.infect, 1
    | Clean -> Grid.infect, 1

  let turnDir = function
    | Clean -> Ccw
    | Weakened -> Front
    | Infected -> Cw
    | Flagged -> Front

  let solve input =
    let grid, start = Parse.grid input
    walk turnDir flip 10000 0 start Up grid |> sprintf "%i"

module B =
  open Virus
  let flip pos = 
    Grid.stateOf pos
    >>
    function
    | Infected -> Grid.flag, 0
    | Flagged -> Grid.clean, 0
    | Weakened -> Grid.infect, 1
    | Clean -> Grid.weaken, 0

  let turnDir = function
    | Clean -> Ccw
    | Weakened -> Front
    | Infected -> Cw
    | Flagged -> Reverse

  let solve input =
    let grid, start = Parse.grid input
    walk turnDir flip 10000000 0 start Up grid |> sprintf "%i"
  

let todo (_ : string list) = "todo"

let solvers = A.solve, B.solve