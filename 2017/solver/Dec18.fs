module TLycken.AdventOfCode.Solutions.Dec18

type Value = int64
let zero = 0L
let one = 1L
type Name = char
type Reference = Number of Value | Register of Name

type Registers = Map<Name,Value>

module Registers =
  let empty = Map.empty : Registers

  let find reg registers =
    match Map.tryFind reg registers with
    | Some value -> value
    | None -> zero : Value

  let resolve ref registers =
    match ref with
    | Number i -> i
    | Register r -> find r registers

  let set reg ref registers =
    let value = resolve ref registers
    Map.add reg value registers

  let private mutate reg ref op registers =
    let value = find reg registers
    let operand = resolve ref registers
    let value' = op value operand
    set reg (Number value') registers

  let add reg ref registers = mutate reg ref (+) registers

  let mul reg ref registers = mutate reg ref (*) registers

  let modulo reg ref registers =
    let value = find reg registers
    let operand = resolve ref registers
    let value' = value % operand
    set reg (Number value') registers

module A =

  type Instruction =
  | Sound of Reference
  | Set of Name * Reference
  | Add of Name * Reference
  | Mul of Name * Reference
  | Mod of Name * Reference
  | Recover of Reference
  | Jump of Reference * Reference

  type State = {
    registers : Registers
    current : int
    instructions : Instruction list
    lastHeard : Value option
    recovered : Value option
  }

  module State =
    let init instructions = {
      registers = Registers.empty
      current = 0
      instructions = instructions
      lastHeard = None
      recovered = None
    }

    let jump cond offset state =
      let offset' =
        if Registers.resolve cond state.registers > zero
        then Registers.resolve offset state.registers
        else 1L
      { state with current = state.current + int offset' }

    let sound ref state =
      let freq = Registers.resolve ref state.registers
      { state with lastHeard = Some freq }

    let recover ref state =
      if Registers.resolve ref state.registers <> zero
      then
        { state with recovered = state.lastHeard }
      else state

    let step state = { state with current = state.current + 1 }

    let private update state op = { state with registers = op state.registers }

    let apply state = function
      | Set (r, ref) -> Registers.set r ref |> update state |> step
      | Add (r, ref) -> Registers.add r ref |> update state |> step
      | Mul (r, ref) -> Registers.mul r ref |> update state |> step
      | Mod (r, ref) -> Registers.modulo r ref |> update state |> step
      | Jump (cond, offset) -> jump cond offset state
      | Sound ref -> sound ref state |> step
      | Recover ref -> recover ref state |> step

    let hasNext state = 0 <= state.current && state.current < List.length state.instructions

  module Parse =
    open TLycken.AdventOfCode.Utils

    let reference = function
      | Regex.Match "(-?\d+)" [i] -> Parse.int64 i |> Option.bind (Number >> Some)
      | Regex.Match "([a-z])" [r] -> String.asChars r |> List.head |> Register |> Some
      | _ -> None

    let char = function
      | Regex.Match "([a-z])" [c] -> String.asChars c |> List.head |> Some
      | _ -> None

    let private binary instr = function
      | [x;y] -> (char x, reference y) |> Option.bind2 |> Option.bind (instr >> Some)
      | _ -> None

    let instruction = function
      | Regex.Match "snd (.+)" [x] -> reference x |> Option.bind (Sound >> Some)
      | Regex.Match "set (.+) (.+)" [x;y] -> binary Set [x;y]
      | Regex.Match "add (.+) (.+)" [x;y] -> binary Add [x;y]
      | Regex.Match "mul (.+) (.+)" [x;y] -> binary Mul [x;y]
      | Regex.Match "mod (.+) (.+)" [x;y] -> binary Mod [x;y]
      | Regex.Match "rcv (.+)" [x] -> reference x |> Option.bind (Recover >> Some)
      | Regex.Match "jgz (.+) (.+)" [x;y] -> (reference x, reference y) |> Option.bind2 |> Option.bind (Jump >> Some)
      | _ -> None

  let rec applyAll state =
    match state.recovered with
    | Some freq -> Some freq
    | None ->
      if State.hasNext state
      then
        let next = state.instructions.[state.current]
        // printfn "%A" { state with instructions = [] }
        // printfn "next: %A" next
        // System.Console.ReadKey() |> ignore
        let state' = State.apply state next
        applyAll state'
      else
        None


  let solve = List.choose Parse.instruction >> State.init >> applyAll >> function Some i -> sprintf "%i" i | _ -> "(silence...)"

module B =
  open TLycken.AdventOfCode.Utils

  type Instruction =
  | Set of Name * Reference
  | Add of Name * Reference
  | Mul of Name * Reference
  | Mod of Name * Reference
  | Jgz of Reference * Reference
  | Snd of Reference
  | Rcv of Name

  module Parse =
    let reference = function
      | Regex.Match "(-?\d+)" [i] -> Parse.int64 i |> Option.bind (Number >> Some)
      | Regex.Match "([a-z])" [r] -> String.asChars r |> List.head |> Register |> Some
      | _ -> None

    let name = function
      | Regex.Match "([a-z])" [c] -> String.asChars c |> List.head |> Some
      | _ -> None

    let binary op = function [n;r] -> (name n, reference r) |> Option.bind2 |> Option.bind (op >> Some) | _ -> None

    let instruction = function
      | Regex.Match "set (.+) (.+)" [x;y] -> binary Set [x;y]
      | Regex.Match "add (.+) (.+)" [x;y] -> binary Add [x;y]
      | Regex.Match "mul (.+) (.+)" [x;y] -> binary Mul [x;y]
      | Regex.Match "mod (.+) (.+)" [x;y] -> binary Mod [x;y]
      | Regex.Match "jgz (.+) (.+)" [x;y] -> (reference x, reference y) |> Option.bind2 |> Option.bind (Jgz >> Some)
      | Regex.Match "snd (.+)" [x] -> reference x |> Option.bind (Snd >> Some)
      | Regex.Match "rcv (.+)" [x] -> name x |> Option.bind (Rcv >> Some)
      | _ -> None

    let instructions = List.choose instruction
  type Id = Zero | One
  type Machine = {
    registers : Registers
    position : int
    inbox : Queue<Value>
    id : Id
    sent : int
  }
  module Machine =
    let init id = {
      registers = Registers.empty |> Registers.set 'p' (match id with Zero -> Number 0L | One -> Number 1L)
      position = 0
      inbox = Queue.empty
      id = id
      sent = 0
    }

    let step s m = { m with position = m.position + s }

    let private bin op reg ref m = { m with registers = op reg ref m.registers }

    let set reg ref = bin Registers.set reg ref >> step 1
    let add reg ref = bin Registers.add reg ref >> step 1
    let mul reg ref = bin Registers.mul reg ref >> step 1
    let modulo reg ref = bin Registers.modulo reg ref >> step 1
    let resolve ref m = Registers.resolve ref m.registers
    let jgz reg ref m =
      let offset =
        if Registers.resolve reg m.registers > 0L
        then Registers.resolve ref m.registers
        else 1L
      step (int offset) m
    let send m = { m with sent = m.sent + 1 } |> step 1

    let oob n i = i < 0 || n <= i

    let waiting m (instructions : Instruction list) = match instructions.[m.position] with Rcv _ -> true | _ -> false

    let isBlocked m instructions =
      let n = List.length instructions

      let wantsToRead m =
        match instructions.[m.position] with
        | Rcv _ -> true
        | _ -> false
      let canRead m = not <| Queue.isEmpty m.inbox
      oob n m.position || (wantsToRead m && not <| canRead m)

    let readMsg m =
      match Queue.dequeue m.inbox with
      | Some (v, inbox') -> Some (v, { m with inbox = inbox' })
      | None -> None

    let addMsg m v = { m with inbox = Queue.enqueue m.inbox v }

    let next m instructions =
      let n = List.length instructions
      if oob n m.position
      then None
      else Some instructions.[m.position]

  module State =
    let init = Machine.init Zero, Machine.init One

    let rec evolve instructions (ma, mb) =
      let nxt = Machine.next ma instructions 
      // printfn "%A %i: %A %A" m1.id m1.position nxt m1.registers
      match nxt with
      | Some (Set (reg, ref)) -> (Machine.set reg ref ma, mb) |> evolve instructions
      | Some (Add (reg, ref)) -> (Machine.add reg ref ma, mb) |> evolve instructions
      | Some (Mul (reg, ref)) -> (Machine.mul reg ref ma, mb) |> evolve instructions
      | Some (Mod (reg, ref)) -> (Machine.modulo reg ref ma, mb) |> evolve instructions
      | Some (Jgz (reg, ref)) -> (Machine.jgz reg ref ma, mb) |> evolve instructions
      | Some (Snd ref) -> send instructions ma mb ref
      | Some (Rcv reg) -> receive instructions ma mb reg
      | None -> wait instructions ma mb
    and send instructions ma mb ref =
      let ma' = Machine.send ma
      let mb' = Machine.resolve ref ma |> Machine.addMsg mb
      evolve instructions (ma', mb')
    and receive instructions ma mb reg =
      let data = Machine.readMsg ma
      match data with
      | Some (v, ma') -> (Machine.set reg (Number v) ma', mb) |> evolve instructions
      | None -> wait instructions ma mb
    and wait instructions ma mb =
      if Machine.isBlocked mb instructions
      then
        printfn "%A %A\n%A %A" ma (Machine.next ma instructions) mb (Machine.next mb instructions)
        match ma.id, mb.id with
        | Zero, One -> mb.sent
        | One, Zero -> ma.sent
        | _ -> failwith "Machines have same id"
      else
        printfn "handover from %A to %A" ma.id mb.id
        evolve instructions (mb, ma)

  let solve = Parse.instructions >> (fun instrs -> State.evolve instrs State.init) >> sprintf "%i"

let solvers = A.solve, B.solve
