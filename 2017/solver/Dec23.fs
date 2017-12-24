module TLycken.AdventOfCode.Solutions.Dec23

open TLycken.AdventOfCode.Utils

type Value = int64
let zero = 0L
let one = 1L
type Name = string
type Reference = Number of Value | Register of Name

type Registers = System.Collections.Generic.Dictionary<Name,Value> //Map<Name,Value>

module Registers =
  let empty = System.Collections.Generic.Dictionary<Name,Value>() : Registers

  let find reg (registers : Registers) =
    if registers.ContainsKey reg
    then registers.[reg]
    else zero
    // match Map.tryFind reg registers with
    // | Some value -> value
    // | None -> zero : Value

  let resolve ref registers =
    match ref with
    | Number i -> i
    | Register r -> find r registers

  let set reg ref registers =
    let value = resolve ref registers
    // Map.add reg value registers
    registers.[reg] <- value

  let private mutate reg ref op registers =
    let value = find reg registers
    let operand = resolve ref registers
    let value' = op value operand
    set reg (Number value') registers

  let add reg ref registers = mutate reg ref (+) registers

  let sub reg ref registers = mutate reg ref (-) registers

  let mul reg ref registers = mutate reg ref (*) registers

  let modulo reg ref registers =
    let value = find reg registers
    let operand = resolve ref registers
    let value' = value % operand
    set reg (Number value') registers

  let mulsub reg a b registers =
    let value = find reg registers
    let a' = resolve a registers
    let b' = resolve b registers
    set reg (Number <| value * a' - b') registers

  let setsub reg a b registers =
    let a' = resolve a registers
    let b' = resolve b registers
    set reg (Number <| a' - b') registers

type Instruction =
| Set of Name * Reference
| Sub of Name * Reference
| Mul of Name * Reference
| Jnz of Reference * Reference
| MulSub of Name * Reference * Reference
| SetSub of Name * Reference * Reference

type Interpreter = {
  instructions : Instruction list
  current : int
  registers : Registers
}

module Parse =

  let reference = function
    | Regex.Match "([a-z])" [reg] -> Some <| Register reg
    | Regex.Match "(-?[0-9]+)" [v] -> Parse.int64 v |> Option.bind (Number >> Some)
    | x -> failwithf "failed to parse reference from %s" x

  let instr i = Option.bind2 >> Option.bind (i >> Some)

  let instruction = function
    | Regex.Match "set ([a-z]) ([\-0-9a-z]+)" [reg; ref] -> (Some <| (reg : Name), reference ref) |> instr Set
    | Regex.Match "sub ([a-z]) ([\-0-9a-z]+)" [reg; ref] -> (Some reg, reference ref) |> instr Sub
    | Regex.Match "mul ([a-z]) (\w+)" [reg; ref] -> (Some reg, reference ref) |> instr Mul
    | Regex.Match "jnz (\w+) ([\-0-9a-z]+)" [a; b] -> (reference a, reference b) |> instr Jnz
    | Regex.Match "mulsub (\w+) (.+) (.+)" [reg; a; b] -> (Some reg, reference a, reference b) |> Option.bind3 |> Option.bind (MulSub >> Some)
    | Regex.Match "setsub (\w+) (.+) (.+)" [reg; a; b] -> (Some reg, reference a, reference b) |> Option.bind3 |> Option.bind (SetSub >> Some)
    | Regex.Match "^#" _ -> None
    | Regex.Match "^$" _ -> None
    | x -> failwithf "failed to parse instruction from %s" x

module Interpreter =
  let init instructions = { instructions = instructions; current = 0; registers = Registers.empty }

  let step i interpreter = { interpreter with current = interpreter.current + i }

  let mutate op reg ref interpreter =
    op reg ref interpreter.registers
    interpreter |> step 1
  let set = mutate Registers.set

  let sub = mutate Registers.sub
  let mul = mutate Registers.mul
  let mulSub reg a b interpreter =
    Registers.mulsub reg a b interpreter.registers
    interpreter |> step 1

  let setSub reg a b interpreter =
    Registers.setsub reg a b interpreter.registers
    interpreter |> step 1
  let jnz a b interpreter =
    let i =
      if Registers.resolve a interpreter.registers <> zero
      then Registers.resolve b interpreter.registers
      else one
    step (int i) interpreter

  let isDone interpreter = interpreter.current < 0 || List.length interpreter.instructions <= interpreter.current

  let current interpreter = if isDone interpreter then None else Some (interpreter.instructions.[interpreter.current])

module A =
  let rec apply (interpreter, multiplications) = interpreter |> Interpreter.current |> function
    | Some (Set (reg, ref)) -> apply (Interpreter.set reg ref interpreter, multiplications)
    | Some (Sub (reg, ref)) -> apply (Interpreter.sub reg ref interpreter, multiplications)
    | Some (Mul (reg, ref)) -> apply (Interpreter.mul reg ref interpreter, multiplications + 1)
    | Some (Jnz (a, b)) -> apply (Interpreter.jnz a b interpreter, multiplications)
    | Some (MulSub (reg, a, b)) -> apply (Interpreter.mulSub reg a b interpreter, multiplications + 1)
    | Some (SetSub (reg, a, b)) -> apply (Interpreter.setSub reg a b interpreter, multiplications)
    | None -> multiplications

  let solve = List.choose Parse.instruction >> Interpreter.init >> (fun interpreter -> apply (interpreter, 0)) >> sprintf "%i"

module B =

  let rec isComposite i d =
    if i = d then false
    elif i % d = 0 then true
    else isComposite i (d+1)

  let rec composite i c h =
    if i = c then h + 1
    else
      if isComposite i 2
      then composite (i+17) c (h+1)
      else composite (i+17) c h

  let optimizedAssembly (_ : string list) =
    let mutable a = 1
    let mutable b = 0
    let mutable c = 0
    let mutable f = 0
    let mutable g = 0
    let mutable h = 0

    if a <> 0
    then
      b <- 108400
      c <- b + 17000
    else
      b <- 84
      c <- 84
    
    for i in [ b..17..c ] do
      f <- 1
      for d = 2 to i-1 do
        g <- i % d
        if g = 0 then f <- 0
      if f = 0 then h <- h + 1

    sprintf "%i" h

  let solve (_ : string list) = composite 108400 (108400+17000) 0 |> sprintf "%i"
  // let solve = optimizedAssembly
let solvers = A.solve, B.solve