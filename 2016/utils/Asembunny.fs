namespace AoC.Utils

module Asembunny =

    type Register =
        | A
        | B
        | C
        | D

    type Memory = { A: int; B: int; C: int; D: int }

    type Value =
        | Constant of int
        | Register of Register

    let get value memory =
        match value with
        | Constant i -> i
        | Register A -> memory.A
        | Register B -> memory.B
        | Register C -> memory.C
        | Register D -> memory.D

    let set register value machine =
        match register with
        | A -> { machine with A = get value machine }
        | B -> { machine with B = get value machine }
        | C -> { machine with C = get value machine }
        | D -> { machine with D = get value machine }

    type Copy = { Src: Value; Dest: Value }
    type Jump = { Condition: Value; Steps: Value }
    type Multiply = Register * Register * Register

    type Instruction =
        | Multiply of Multiply
        | Copy of Copy
        | Jump of Jump
        | Increment of Value
        | Decrement of Value
        | Toggle of Value
        | Noop
        | Out of Value

    type State =
        { Current: int
          Memory: Memory
          Program: Map<int, Instruction>
          Output: int -> unit }

    let showv =
        function
        | Register r ->
            match r with
            | A -> "a"
            | B -> "b"
            | C -> "c"
            | D -> "d"
        | Constant c -> string c

    let show =
        function
        | Copy { Src = a; Dest = b } -> sprintf "cpy %s %s" (showv a) (showv b)
        | Jump { Condition = a; Steps = b } -> sprintf "jnz %s %s" (showv a) (showv b)
        | Increment a -> sprintf "inc %s" (showv a)
        | Decrement a -> sprintf "dec %s" (showv a)
        | Toggle x -> sprintf "tgl %s" (showv x)
        | Multiply (a, x, y) -> sprintf "mul %s %s %s" (showv (Register a)) (showv (Register x)) (showv (Register y))
        | Noop -> "nop"
        | Out x -> sprintf "out %s" (showv x)


    let init a b c d program =
        { Current = 0
          Memory = { A = a; B = b; C = c; D = d }
          Program =
              program
              // TODO: rewrite as a folder that takes multiwidth of mul instructions into account
              |> Seq.zip (Seq.initInfinite id)
              |> Map.ofSeq
          Output = ignore }


    let mov steps state =
        { state with
              Current = state.Current + steps }

    let toggleAt i =
        Map.change i
        <| function
        | Some (Increment v) -> Some(Decrement v)
        | Some (Decrement v) -> Some(Increment v)
        | Some (Toggle v) -> Some(Increment v)
        | Some (Out v) -> Some(Increment v)
        | Some (Copy { Src = a; Dest = b }) -> Some(Jump { Condition = a; Steps = b })
        | Some (Jump { Condition = a; Steps = b }) -> Some(Copy { Src = a; Dest = b })
        | Some (Multiply args) -> Some(Multiply args)
        | Some Noop -> Some Noop
        | None -> None


    let apply state =
        function
        | Copy { Src = v; Dest = Register r } ->
            mov
                1
                { state with
                      Memory = set r v state.Memory }

        | Copy c ->
            printfn "@%d: ignoring invalid copy %A" state.Current c
            mov 1 state

        | Jump { Condition = c; Steps = s } ->
            let steps =
                if (get c state.Memory) <> 0 then (get s state.Memory) else 1

            mov steps state

        | Increment (Register r) ->
            mov
                1
                { state with
                      Memory = set r (Constant((get (Register r) state.Memory) + 1)) state.Memory }

        | Increment i ->
            printfn "@%d: ignoring invalid increment %A" state.Current i
            mov 1 state

        | Decrement (Register r) ->
            mov
                1
                { state with
                      Memory = set r (Constant((get (Register r) state.Memory) - 1)) state.Memory }

        | Decrement d ->
            printfn "@%d: ignoring invalid decrement %A" state.Current d
            mov 1 state

        | Toggle x ->
            mov
                1
                { state with
                      Program =
                          state.Program
                          |> toggleAt (state.Current + get x state.Memory) }

        | Multiply (a, x, y) ->
            let m = get (Register a) state.Memory
            let x' = get (Register x) state.Memory
            let y' = get (Register y) state.Memory

            let a' = Constant(m + x' * y')

            let mem' =
                state.Memory
                |> set a a'
                |> set x (Constant 0)
                |> set y (Constant 0)

            mov 1 { state with Memory = mem' }
        | Noop -> mov 1 state
        | Out v ->
            let x = get v state.Memory
            state.Output x
            mov 1 state

    let rec run (state: State) =
        // printfn
        //     "%d: (%d, %d, %d, %d) %s"
        //     state.Current
        //     state.Memory.A
        //     state.Memory.B
        //     state.Memory.C
        //     state.Memory.D
        //     (if state.Program.ContainsKey(state.Current) then show state.Program.[state.Current] else "EOF")

        if state.Program.ContainsKey state.Current
        then run (apply state state.Program.[state.Current])
        else state.Memory


    module Parse =
        let private register =
            function
            | "a" -> Some A
            | "b" -> Some B
            | "c" -> Some C
            | "d" -> Some D
            | _ -> None
            >> Option.map Register

        let private constant = Int.parse >> Option.map Constant

        let private copy src dest =
            match src, dest with
            | Some src', Some dest' -> Some(Copy { Src = src'; Dest = dest' })
            | _ -> None

        let private jump cond steps =
            match cond, steps with
            | Some c', Some s' -> Some(Jump { Condition = c'; Steps = s' })
            | _ -> None

        let private increment = Option.map Increment

        let private decrement = Option.map Decrement

        let private toggle = Option.map Toggle

        let private multiply a x y =
            match (a, x, y) with
            | Some (Register a'), Some (Register x'), Some (Register y') -> Some(Multiply(a', x', y'))
            | _ -> None

        let private out = Option.map Out


        let instruction =
            function
            | Regex.Match @"^cpy (-?\d+) (-?\d+)$" [ v; r ] -> copy (constant v) (constant r)
            | Regex.Match @"^cpy (-?\d+) (\w)$" [ v; r ] -> copy (constant v) (register r)
            | Regex.Match @"^cpy (\w) (-?\d+)$" [ v; r ] -> copy (register v) (constant r)
            | Regex.Match @"^cpy (\w) (\w)$" [ src; dst ] -> copy (register src) (register dst)
            | Regex.Match @"^jnz (-?\d+) (-?\d+)$" [ cond; steps ] -> jump (constant cond) (constant steps)
            | Regex.Match @"^jnz (\w) (-?\d+)$" [ cond; steps ] -> jump (register cond) (constant steps)
            | Regex.Match @"^jnz (-?\d+) (\w)$" [ cond; steps ] -> jump (constant cond) (register steps)
            | Regex.Match @"^jnz (\w) (\w)$" [ cond; steps ] -> jump (register cond) (register steps)
            | Regex.Match @"^inc (\w)$" [ r ] -> increment (register r)
            | Regex.Match @"^dec (\w)$" [ r ] -> decrement (register r)
            | Regex.Match @"^tgl (\w)$" [ r ] -> toggle (register r)
            | Regex.Match @"^tgl (\d+)$" [ steps ] -> toggle (constant steps)
            | Regex.Match @"^mul (\w) (\w) (\w)$" [ a; x; y ] -> multiply (register a) (register x) (register y)
            | Regex.Match @"^out (\w)$" [ r ] -> out (register r)
            | Regex.Match @"^out (-?\d+)$" [ c ] -> out (constant c)
            | "nop" -> Some Noop
            | i -> failwithf "failed to parse %s" i

    let parse supportMultiply: string -> Instruction option =
        function
        | s when s.StartsWith "/" && not supportMultiply -> Parse.instruction (s.Substring 2)
        | s when s.StartsWith "/" && supportMultiply -> None
        | s when s.StartsWith "#" && supportMultiply -> Parse.instruction (s.Substring 2)
        | s when s.StartsWith "#" && not supportMultiply -> None
        | s -> Parse.instruction s
