namespace AoC.Dec1

module Domain =

    open AoC.Utils.Types

    type State = {
        here : Position
        facing : Direction
        prev : State option
    }
    type Instruction = {
        turn : Turn
        steps : int
    }

    let turn d s = { s with facing = turn s.facing d }
    
    let move1 s = { s with here = move1 s.facing s.here; prev = Some s }

    let distance s = abs(s.here.x) + abs(s.here.y)

    let rec crosses pos s =
        match s with
        | None -> false
        | Some state -> state.here = pos || crosses pos state.prev

    module A =
    
        let rec move i s =
            match i with
            | 0 -> s
            | _ -> move (i-1) (move1 s)

        let follow s i = (turn i.turn >> move i.steps) s

        let followFrom = Seq.fold follow

    module B =
        let isCrossing s = crosses s.here s.prev

        let stopIfCrossing s next =
            if isCrossing s
            then s
            else next s

        let rec move i s =
            match i with
            | 0 -> s
            | _ -> stopIfCrossing (move1 s) (move (i-1))

        let follow s i = (turn i.turn >> move i.steps) s

        let rec followFrom state instrs =
            match instrs with
            | [] -> state
            | i::is -> stopIfCrossing (follow state i) (fun s -> followFrom s is)

module Parsing =

    open AoC.Utils.Types
    open AoC.Utils
    open Domain
    
    let stepcountFromChars = String.join >> Int.parse

    let instructionFromString input =
        match String.asChars input with
        | 'R'::digits -> (Some Right, stepcountFromChars digits)
        | 'L'::digits -> (Some Left, stepcountFromChars digits)
        | _ -> (None, None)

    let parse data =
        match instructionFromString data with
        | (Some t, Some s) -> Some { turn = t; steps = s }
        | _ -> None

    let instructions = String.split ", " >> List.choose parse

module Solutions =

    open Domain
    open Parsing
    open AoC.Utils.Types

    let initState = { here = { x = 0; y = 0 }; facing = North; prev = None }

    module A =

        open Domain.A
        let solve = List.head >> instructions >> followFrom initState >> distance >> sprintf "%d"

    module B =

        open Domain.B
        let solve = List.head >> instructions >> followFrom initState >> distance >> sprintf "%d"
