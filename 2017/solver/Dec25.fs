module TLycken.AdventOfCode.Solutions.Dec25

type Tape = Map<int,bool>

module Tape =
  let empty = Map.empty : Tape

  let write = Map.add : int -> bool -> Tape -> Tape

  let read i = (Map.tryFind i >> function Some x -> x | None -> false) : Tape -> bool

  let checksum = Map.toList >> List.filter snd >> List.length


type WriteAction = bool
type MoveDir = Right | Left
type MoveAction = MoveDir
type StateName = A | B | C | D | E | F
type NextAction = StateName

type ChoiceAction = WriteAction * MoveAction * NextAction
type State = ChoiceAction * ChoiceAction * int * Tape

module Parse =
  open TLycken.AdventOfCode.Utils

  let state = function | "A" -> Some A | "B" -> Some B | "C" -> Some C | "D" -> Some D | "E" -> Some E | "F" -> Some F | _ -> None
  let init = function
    | Regex.Match "Begin in state (\w)" [s] -> state s
    | _ -> None
  let nsteps = function
    | Regex.Match "Perform a diagnostic checksum after (\d+) steps." [n] -> Parse.int n
    | _ -> None

  let preamble = function
    | [i;n] -> (init i, nsteps n) |> Option.bind2
    | _ -> None

  let transitionStart = function
    | Regex.Match "In state (\w):" [s] -> state s
    | _ -> None
  let direction = function
    | "right" -> Some Right
    | "left" -> Some Left
    | _ -> None
  let choice = function
    | (
        Regex.Match "Write the value (\d)." [w],
        Regex.Match "Move one slot to the (.+)\." [dir],
        Regex.Match "Continue with state (\w)" [s]
      ) ->
        (
          w |> Parse.int |> Option.bind ((=) 1 >> Some),
          direction dir,
          state s
        ) |> Option.bind3
    | _ -> None
  let transition = function
    | [Regex.Match "In state (\w)." [from];_;w0;m0;c0;_;w1;m1;c1] ->
      (
        from
        |> state,
        (
          (w0,m0,c0) |> choice,
          (w1,m1,c1) |> choice
        )
        |> Option.bind2
      ) |> Option.bind2
    | x -> failwithf "Couldn't parse transition from %A" x

  let transitions = List.filter (String.length >> (<) 0) >> List.chunkBySize 9 >> List.choose transition >> Map.ofList

module TuringMachine =
  let move i = function Right -> i + 1 | Left -> i - 1

  let step1 choices ((ifZero, ifOne), tape, pos) =
    let w, m, n = if Tape.read pos tape then ifOne else ifZero
    let tape' = Tape.write pos w tape
    let pos' = move pos m
    let ifZero', ifOne' = choices n
    (ifZero', ifOne'), tape', pos'

  let rec step choices steps state =
    if steps = 0
    then state
    else
      let state' = step1 choices state
      step choices (steps-1) state'

let choices transitions state = Map.find state transitions

let solve input =
  let firstState, n =
    match input |> List.take 2 |> Parse.preamble with
    | Some (s, n) -> s, n
    | None -> failwith "Failed to parse preamble"
  let transitions = input |> List.skip 2 |> Parse.transitions

  if not <| Map.containsKey firstState transitions
  then failwith "Failed to parse transitions"

  let init = choices transitions firstState, Tape.empty, 0
  let final = TuringMachine.step (choices transitions) n init

  let (_, tape, _) = final

  let checksum = Tape.checksum tape

  sprintf "%i" checksum

let final (_ : string list) = "Merry christmas!"

let solvers = solve, final