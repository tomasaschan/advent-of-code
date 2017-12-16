module TLycken.AdventOfCode.Solutions.Dec16

open TLycken.AdventOfCode.Utils
open TLycken.AdventOfCode.Solutions.Dec8.Parse

type Instruction =
| Spin of int
| Exchange of int * int
| Partner of char * char

module Parse =

  type PartialInstruction = PSpin of int option | PExchange of int option * int option | PPartner of char * char

  let instruction =
    function
    | Regex.Match "s(\d+)" [steps] -> PSpin (Parse.int steps) |> Some
    | Regex.Match "x(\d+)/(\d+)" [A;B] -> PExchange (Parse.int A, Parse.int B) |> Some
    | Regex.Match "p([a-p])/([a-p])" [A;B] -> PPartner (A |> asChars |> List.head, B |> asChars |> List.head) |> Some
    | _ -> None
    >>
    function
    | Some (PSpin (Some steps)) -> Some (Spin steps)
    | Some (PExchange (Some A, Some B)) -> Some (Exchange (A,B))
    | Some (PPartner (A, B)) -> Some (Partner (A, B))
    | _ -> None

  let instructions = splitString [|','|] >> List.choose instruction

let applySpin s state =
  let n = List.length state
  List.permute (fun i -> (i + s) % n) state

let applyExchange a b =
  List.permute <| fun i ->
    if i = a then b
    elif i = b then a
    else i

let applyPartner a b state =
  let a' = Seq.findIndex ((=) a) state
  let b' = Seq.findIndex ((=) b) state
  applyExchange a' b' state

let apply state = function
  | Spin s -> applySpin s state
  | Exchange (a,b) -> applyExchange a b state
  | Partner (a,b) -> applyPartner a b state

let dance state instructions = List.fold apply state instructions

let output state = state |> Seq.map (sprintf "%c") |> String.concat ""

let rec cycleLength len init state instructions =
  if len <> 0 && init = state then len
  else
    let state' = dance state instructions
    cycleLength (len+1) init state' instructions

let rec danceAgain n state instructions =
  if n = 0 then state
  else
    let n' = n-1
    let state' = dance state instructions
    danceAgain n' state' instructions

module A =

  let solve init = List.head >> Parse.instructions >> dance init >> output

module B =

  let solve init rounds input =
    let instrs = input |> List.head |> Parse.instructions
    let cycle = cycleLength 0 init init instrs
    let n = rounds % cycle
    danceAgain n init instrs |> output

let initialState n = List.init n (fun i -> char ((int 'a') + i))

let solvers = A.solve (initialState 16), B.solve (initialState 16) 1_000_000_000
