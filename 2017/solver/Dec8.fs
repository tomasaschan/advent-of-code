module TLycken.AdventOfCode.Solutions.Dec8

type ConditionData = { lhs : string; rhs : int }
type Condition = Gt of ConditionData | Lt of ConditionData | Ge of ConditionData | Le of ConditionData | Eq of ConditionData | Ne of ConditionData
type InstructionData = { reg : string; operand : int; condition : Condition }
type Instruction = Inc of InstructionData | Dec of InstructionData

module Parse =
  open TLycken.AdventOfCode.Utils

  let operation input data =
    match input with
    | "<" -> Lt data |> Some
    | ">" -> Gt data |> Some
    | "<=" -> Le data |> Some
    | ">=" -> Ge data |> Some
    | "==" -> Eq data |> Some
    | "!=" -> Ne data |> Some
    | _ -> printfn "Couln't parse operator operator %s" input; None

  let condition input =
    match input with
    | Regex.Match "if (\w+) ([<>=!]+) (-?\d+)" [reg;op;amount] ->
      match Parse.int amount with
      | Some a -> operation op { lhs = reg; rhs = a }
      | _ -> printfn "Couldn't parse instruction %s" input; None
    | _ -> printfn "Couldn't parse condition %s" input; None

  let instruction input =
    match input with
    | Regex.Match "^(\w+) (inc|dec) (-?\d+) (.+)$" [reg;instr;am;cond] ->
      match Parse.int am, condition cond with
      | Some am', Some cond' ->
        match instr with
        | "inc" -> Inc { reg = reg; operand = am'; condition = cond' } |> Some
        | "dec" -> Dec { reg = reg; operand = am'; condition = cond' } |> Some
        | _ -> printfn "Couldn't parse instruction %s" instr; None
      | _ -> printfn "Couldn't parse amount, condition %s, %s" am cond; None
    | _ -> printfn "Couldn't parse line %s" input; None

module Registers =

  let empty = Map.empty

  let find name registers =
    match Map.tryFind name registers with
    | Some i -> i
    | None -> 0

  let inc name amount register = Map.add name (find name register + amount) register

  let dec name amount register = inc name (-amount) register

  let isTrue registers = function
    | Gt { lhs = lhs; rhs = rhs } -> (find lhs registers) > rhs
    | Lt { lhs = lhs; rhs = rhs } -> (find lhs registers) < rhs
    | Ge { lhs = lhs; rhs = rhs } -> (find lhs registers) >= rhs
    | Le { lhs = lhs; rhs = rhs } -> (find lhs registers) <= rhs
    | Eq { lhs = lhs; rhs = rhs } -> (find lhs registers) = rhs
    | Ne { lhs = lhs; rhs = rhs } -> (find lhs registers) <> rhs

  let apply registers = function
    | Inc { reg = name; operand = amount; condition = cond } ->
      if isTrue registers cond
      then inc name amount registers
      else registers
    | Dec { reg = name; operand = amount; condition = cond } ->
      if isTrue registers cond
      then dec name amount registers
      else registers
    
  let rec applyAll registers highestSeen = function
    | [] -> registers, highestSeen
    | instruction :: rest ->
      let registers' = apply registers instruction
      let highest = registers' |> Map.toList |> List.map snd |> List.max
      applyAll registers' (List.max [highest; highestSeen]) rest


module A =
  let solve = List.choose Parse.instruction >> List.fold Registers.apply Registers.empty >> Map.toList >> List.map snd >> List.max >> sprintf "%i"

module B =
  let solve = List.choose Parse.instruction >> Registers.applyAll Registers.empty 0 >> snd >> sprintf "%i"
let solvers = A.solve, B.solve