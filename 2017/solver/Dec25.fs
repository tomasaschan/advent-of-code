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

module TuringMachine =
  let move i = function Right -> i + 1 | Left -> i - 1

  let choices = function
    | A -> (true, Right, B), (false, Left, E)
    | B -> (true, Left, C), (false, Right, A)
    | C -> (true, Left, D), (false, Right, C)
    | D -> (true, Left, E), (false, Left, F)
    | E -> (true, Left, A), (true, Left, C)
    | F -> (true, Left, E), (true, Right, A) 

  let step1 ((ifZero, ifOne), tape, pos) =
    let w, m, n = if Tape.read pos tape then ifOne else ifZero
    let tape' = Tape.write pos w tape
    let pos' = move pos m
    let ifZero', ifOne' = choices n
    (ifZero', ifOne'), tape', pos'

  let rec step steps state =
    if steps = 0
    then state
    else
      let state' = step1 state
      step (steps-1) state'
      

module A =
  let solve input =
    let init = TuringMachine.choices A, Tape.empty, 0
    let final = TuringMachine.step 12386363 init

    let (_, tape, _) = final

    let checksum = Tape.checksum tape

    sprintf "%i" checksum

let todo (_ : string list) = "todo"

let solvers = A.solve, todo