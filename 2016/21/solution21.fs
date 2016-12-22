namespace AoC.Dec21

open AoC.Utils

module Domain =

    type Direction = Left | Right

    type Instruction = 
    | SwapPosition of int * int
    | SwapLetter of string * string
    | RotateSteps of Direction * int
    | RotatePos of string
    | Reverse of int * int
    | Move of int * int

    type RuleMachine = {
        swapPos : int -> int -> string -> string
        swapLetter : string -> string -> string -> string
        rotateSteps : Direction -> int -> string -> string
        rotatePos : string -> string -> string
        reverse : int -> int -> string -> string
        move : int -> int -> string -> string
    }

    let private prefix a (str : string) = if a > 0 then str.[0..a-1] else ""

    let private suffix b (str : string) = if b < str.Length - 1 then str.[b+1..str.Length-1] else ""

    let private splitPoint s c t =
        let l = String.length c
        match t with
        | Left -> s % l
        | Right -> (String.length c) - (s % l)

    let swapPos a b (str : string) =
        printfn "swap %d %d %s" a b str
        let a', b' = min a b, max a b
        (prefix a' str) + str.[b'..b'] + str.[a'+1..b'-1] + str.[a'..a'] + (suffix b' str)
   
    let swapLetter (a : string) (b : string) (str : string) =
        printfn "swap %s %s %s" a b str
        swapPos (str.IndexOf a) (str.IndexOf b) str

    let rotateSteps t s str =
        printfn "rotate steps %A %d %s" t s str
        (String.asChars >> List.splitAt (splitPoint s str t) >> (fun t -> [snd t; fst t]) >> List.collect id >> String.join) str

    let rotatePos (a : string) (str : string) =
        printfn "rotate pos %s %s" a str
        let idx = str.IndexOf a
        let str' = rotateSteps Right 1 str
        let str'' = rotateSteps Right idx str'
        if idx >= 4 then
            printfn "idx = %d >= 4; rotating extra step" idx
            rotateSteps Right 1 str''
        else
            printfn "idx = %d < 4; done" idx
            str''
//        rotateSteps Right (idx + if idx >= 4 then 1 else 0) str

    let reverse a b str =
        printfn "reverse %d %d %s" a b str
        let a', b' = min a b, max a b
        (prefix a' str) + (str.[a'..b'] |> String.asChars |> List.rev |> String.join) + (suffix b' str)

    let move a b (str : string) =
        printfn "move %d %d %s" a b str
        let letter = str.[a]
        let without = (prefix a str) + (suffix a str)
        without
        |> String.asChars
        |> List.splitAt b
        |> (fun (pre, post) -> [pre; [letter]; post])
        |> List.collect id
        |> String.join

    let apply rules str = (function
        | SwapPosition (a,b) -> rules.swapPos a b str
        | SwapLetter (a,b) -> rules.swapLetter a b str
        | Reverse (a,b) -> rules.reverse a b str
        | RotateSteps (t,s) -> rules.rotateSteps t s str
        | RotatePos a -> rules.rotatePos a str
        | Move (a,b) -> rules.move a b str) >> (fun s -> printfn "arrived at %s" s; s)

module Parse =

    open Domain
    open AoC.Utils

    let swapPosition a b =
        match Int.parse a, Int.parse b with
        | (Some a', Some b') -> Some (SwapPosition (a', b'))
        | _ -> None

    let swapLetter a b = Some (SwapLetter (a, b))

    let rotate dir steps =
        match dir, Int.parse steps with
        | "right", Some s -> Some (RotateSteps (Right, s))
        | "left", Some s -> Some (RotateSteps (Left, s))
        | _ -> None

    let rotateFromPos pos = Some (RotatePos pos)

    let reverse a b =
        match Int.parse a, Int.parse b with
        | (Some a', Some b') -> Some (Reverse (a', b'))
        | _ -> None

    let move a b =
        match Int.parse a, Int.parse b with
        | (Some a', Some b') -> Some (Move (a', b'))
        | _ -> None

    let instruction = function
        | Regex.Match "^swap position (\d+) with position (\d+)$" [a;b] -> swapPosition a b
        | Regex.Match "^swap letter (\w) with letter (\w)$" [a;b] -> swapLetter a b
        | Regex.Match "^rotate (left|right) (\d+) steps?$" [a;b] -> rotate a b
        | Regex.Match "^rotate based on position of letter (\w)$" [a] -> rotateFromPos a
        | Regex.Match "^reverse positions (\d+) through (\d+)$" [a; b] -> reverse a b
        | Regex.Match "^move position (\d+) to position (\d+)$" [a; b] -> move a b
        | _ -> None

    let instructions = List.choose instruction

module Solver =

    open Domain

    module A =

        let scrambler = {
            swapPos = Domain.swapPos
            swapLetter = Domain.swapLetter
            rotateSteps = Domain.rotateSteps
            rotatePos = Domain.rotatePos
            reverse = Domain.reverse
            move = Domain.move
        }

        let solve pass = Parse.instructions >> List.fold (apply scrambler) pass >> (fun s -> printfn "answer: %s" s; s) >> sprintf "%s"

    module B =

        let other = function
            | Left -> Right
            | Right -> Left

        let origindex = function
            | 1 -> 0
            | 3 -> 1
            | 5 -> 2
            | 7 -> 3
            | 2 -> 4
            | 4 -> 5
            | 6 -> 6
            | 0 -> 7
            | _ -> -1

        let rotatePos (a : string) (str : string) =
            printfn "rotate pos backward %s %s" a str
            let idx' = str.IndexOf a
            let idx = origindex idx'

            let dir = if idx < idx' then Left else Right
            let diff = abs(idx - idx')

            rotateSteps dir diff str


        let unscrambler = {
            swapPos = Domain.swapPos
            swapLetter = Domain.swapLetter
            rotateSteps = (fun t ->
                printfn "rotate steps backwards"
                Domain.rotateSteps (other t))
            rotatePos = rotatePos
            reverse = Domain.reverse
            move = (fun a b ->
                printfn "move backwards"
                Domain.move b a)
        }

        let solve pass = Parse.instructions >> List.rev >> List.fold (apply unscrambler) pass >> sprintf "%s"
