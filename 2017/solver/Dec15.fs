module TLycken.AdventOfCode.Solutions.Dec15

open TLycken.AdventOfCode.Utils

module Parse =

  let parse1 =
    function
    | Regex.Match @"(\d+)" [s] -> Parse.uint64 s
    | _ -> None

  let starts =
    List.choose parse1
    >>
    function
    | [a;b] -> Some (a,b)
    | _ -> None


let rec gen factor divisor x =
  let x' = (factor * x) % 2147483647UL
  if x' % divisor = 0UL
  then x'
  else gen factor divisor x'

let low x = x &&& 0xffffUL

let rec bin =
  function
  | 0UL -> "0"
  | 1UL -> "1"
  | i ->
    let bit = string (i % 2UL)
    (bin (i / 2UL)) + bit

let pad (s : string) = s.PadLeft(32, '0')
let toBinary = bin >> pad

let rec judge genA genB a b count total =
  if total = 0 then count
  else
    let a', b' = genA a, genB b
    let a'', b'' = low a', low b'
    let count' = if a'' = b'' then count + 1 else count
    judge genA genB a' b' count' (total-1)

let solve genA genB N =
  Parse.starts
  >>
  function
  | Some (a,b) -> judge genA genB a b 0 N |> sprintf "%i"
  | _ -> "parsing failed"

module A =
  let genA = gen 16807UL 1UL
  let genB = gen 48271UL 1UL
  let solve = solve genA genB 40_000_000

module B =
  let genA = gen 16807UL 4UL
  let genB = gen 48271UL 8UL

  let solve = solve genA genB 5_000_000

let solvers = A.solve, B.solve