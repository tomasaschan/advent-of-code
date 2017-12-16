module TLycken.AdventOfCode.Solutions.Dec11

open TLycken.AdventOfCode.Utils

type Direction = N | NE | SE | S | SW | NW

module Parse =
  let direction = function
  | "n" -> Some N
  | "ne" -> Some NE
  | "se" -> Some SE
  | "s" -> Some S
  | "sw" -> Some SW
  | "nw" -> Some NW
  | _ -> None

  let directions = String.split "," >> List.choose direction

type Position = { nw : int; n : int; ne : int }
let center = { nw = 0; n = 0; ne = 0 }

let walk from = function
| N -> { from with n = from.n + 1 }
| NE -> { from with ne = from.ne + 1 }
| SE -> { from with nw = from.nw - 1 }
| S -> { from with n = from.n - 1 }
| SW -> { from with ne = from.ne - 1 }
| NW -> { from with nw = from.nw + 1 }

let flip { nw = nw; n = n; ne = ne } = { nw = -nw; n = -n; ne = -ne }

let rec simplify p =
  if p.nw > 0 && p.ne > 0 then
    let r = [p.nw; p.ne] |> List.min
    simplify { nw = p.nw - r; n = p.n + r; ne = p.ne - r }
  elif p.nw < 0 && p.ne < 0 then (flip >> simplify) p
  
  elif p.n > 0 && p.nw < 0 then
    let r = [p.n; -p.nw] |> List.min
    simplify { nw = p.nw + r; n = p.n - r; ne = p.ne + r }
  elif p.n < 0 && p.nw > 0 then (flip >> simplify) p

  elif p.n > 0 && p.ne < 0 then
    let r = [p.n; -p.ne] |> List.min
    simplify { nw = p.nw + r; n = p.n - r; ne = p.ne + r }
  elif p.n < 0 && p.ne > 0 then (flip >> simplify) p

  else p

let distance = simplify >> (fun { nw = nw; n = n; ne = ne } -> [nw; n; ne] |> List.sumBy abs)

module A =
  let rec follow start = function
  | [] -> start
  | move :: moves ->
    let next = walk start move
    follow next moves

  let solve = List.head >> Parse.directions >> follow center >> distance >> sprintf "%i"

module B =
  let rec follow pos max = function
  | [] -> max
  | move :: moves ->
    let pos' = walk pos move
    let dist = distance pos'
    let max' =
      if dist > max
      then dist
      else max
    follow pos' max' moves

  let solve = List.head >> Parse.directions >> follow center 0 >> sprintf "%i"

let solvers = A.solve, B.solve
