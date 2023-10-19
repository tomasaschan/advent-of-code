namespace AoC.Dec8

module Domain =

    let height, width = 6, 50

    type TurnOn = { rows : int; cols : int }
    type ColumnRotation = { col : int; steps : int }
    type RowRotation = { row : int; steps : int }

    type Instruction =
    | TurnOn of TurnOn
    | ColumnRotation of ColumnRotation
    | RowRotation of RowRotation

    let rec private _transpose lists =
        match lists with
        | (_::_)::_ -> List.map List.head lists :: _transpose (List.map List.tail lists)
        | _ -> []
    let private _rotate<'a> i (lst : 'a list) =
        (List.splitAt (lst.Length - i % lst.Length) >> (fun (a,b) -> List.append b a)) lst

    let private _rotateOne rc s =
        List.mapi (fun idx t -> if idx = rc then _rotate s t else t)

    let private _rotateRow k i = _rotateOne k i

    let private _rotateCol k i = _transpose >> _rotateOne k i >> _transpose
    
    let private _turnOnRow k = List.mapi (fun i t -> if i < k then true else t) 

    let private _turnOn rs cs = List.mapi (fun i r -> if i < rs then _turnOnRow cs r else r)

    let update screen instr =
        match instr with
        | RowRotation { row = r; steps = s } -> _rotateRow r s screen 
        | ColumnRotation { col = c; steps = s } -> _rotateCol c s screen
        | TurnOn { rows = r; cols = c } -> _turnOn r c screen

    let total = List.concat >> List.map (fun b -> if b then 1 else 0) >> List.sum

module Parse =

    open AoC.Utils
    open Domain

    type InstructionType = TO | RR | CR

    let private _match str =
        match str with
        | Regex.Match "rect (\d+)x(\d+)" [a;b] -> Some (TO, Int.parse a, Int.parse b)
        | Regex.Match "rotate row y=(\d+) by (\d+)" [a;b] -> Some(RR, Int.parse a, Int.parse b)
        | Regex.Match "rotate column x=(\d+) by (\d+)" [a;b] -> Some(CR, Int.parse a, Int.parse b)
        | _ -> None

    let private _build m =
        match m with
        | Some (TO, Some x, Some y) -> Some (TurnOn { cols = x; rows = y })
        | Some (RR, Some y, Some s) -> Some (RowRotation { row = y; steps = s })  
        | Some (CR, Some x, Some s) -> Some (ColumnRotation { col = x; steps = s })
        | _ -> None

    let instruction = _match >> _build

    let private _display b = if b then "#" else "."


module Show =

    open Domain
    open AoC.Utils

    let pixel p = if p then "#" else "."

    let concat separator a b = sprintf "%s%s%s" a separator b

    let screen = Seq.mapReduce (Seq.mapReduce pixel (concat "")) (concat "\n")

module Solver =

    open Domain

    let initScreen = List.init Domain.height (fun r -> List.init Domain.width (fun c -> false))

    let finalScreen = List.choose Parse.instruction >> List.fold update initScreen

    module A =

        let solve = finalScreen >> total >> sprintf "%d"

    module B =

        let solve = finalScreen >> Show.screen >> sprintf "\n%s\n"