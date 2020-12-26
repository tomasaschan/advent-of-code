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

    type Instruction =
        | Copy of Copy
        | Jump of Jump
        | Increment of Value
        | Decrement of Value

    type State = { Current: int; Memory: Memory }

    let init a b c d =
        { Current = 0
          Memory = { A = a; B = b; C = c; D = d } }

    let mov steps state =
        { state with
              Current = state.Current + steps }

    let jnz state =
        function
        | _ -> mov 1 state

    let apply state =
        function
        | Copy { Src = v; Dest = Register r } ->
            mov
                1
                { state with
                      Memory = set r v state.Memory }
        | Jump { Condition = c; Steps = (Constant s) } ->
            let steps =
                if (get c state.Memory) <> 0 then s else 1

            mov steps state
        | Increment (Register r) ->
            mov
                1
                { state with
                      Memory = set r (Constant((get (Register r) state.Memory) + 1)) state.Memory }
        | Decrement (Register r) ->
            mov
                1
                { state with
                      Memory = set r (Constant((get (Register r) state.Memory) - 1)) state.Memory }
        | _ -> mov 1 state


    let rec run (state: State) (instructions: Instruction list) =
        if state.Current = List.length instructions
        then state.Memory
        else run (apply state instructions.[state.Current]) instructions


    module Parse =
        let private register r =
            match r with
            | "a" -> Some A
            | "b" -> Some B
            | "c" -> Some C
            | "d" -> Some D
            | _ -> None

        let private copyConstant v r =
            match Int.parse v, register r with
            | Some i, Some r' -> Some(Copy { Src = Constant i; Dest = Register r' })
            | _ -> None

        let private copyRegister s d =
            match register s, register d with
            | Some s', Some d' ->
                Some
                    (Copy
                        { Src = Register s'
                          Dest = Register d' })
            | _ -> None

        let private jumpRegister c s =
            match register c, Int.parse s with
            | Some c', Some s' ->
                Some
                    (Jump
                        { Condition = Register c'
                          Steps = Constant s' })
            | _ -> None

        let private jumpConstant c s =
            match Int.parse c, Int.parse s with
            | Some c', Some s' ->
                Some
                    (Jump
                        { Condition = Constant c'
                          Steps = Constant s' })
            | _ -> None

        let private increment r =
            match register r with
            | Some r' -> Some(Increment <| Register r')
            | _ -> None

        let private decrement r =
            match register r with
            | Some r' -> Some(Decrement <| Register r')
            | _ -> None



        let instruction =
            function
            | Regex.Match "^cpy (-?\d+) (\w)$" [ v; r ] -> copyConstant v r
            | Regex.Match "^cpy (\w) (\w)$" [ src; dst ] -> copyRegister src dst
            | Regex.Match "^jnz (-?\d+) (-?\d+)$" [ cond; steps ] -> jumpConstant cond steps
            | Regex.Match "^jnz (\w) (-?\d+)$" [ cond; steps ] -> jumpRegister cond steps
            | Regex.Match "^inc (\w)" [ r ] -> increment r
            | Regex.Match "^dec (\w)" [ r ] -> decrement r
            | _ -> None
