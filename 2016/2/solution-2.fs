namespace AoC.Dec2

module Domain =

    type Move = Right | Left | Up | Down

    module A =
        type Key = One | Two | Three | Four | Five | Six | Seven | Eight | Nine

        let stepUp key =
            match key with
            | One -> One
            | Two -> Two
            | Three -> Three
            | Four -> One
            | Five -> Two
            | Six -> Three
            | Seven -> Four
            | Eight -> Five
            | Nine -> Six
        let stepDown key =
            match key with
            | One -> Four 
            | Two -> Five
            | Three -> Six
            | Four -> Seven
            | Five -> Eight
            | Six -> Nine
            | Seven -> Seven
            | Eight -> Eight
            | Nine -> Nine
        let stepLeft key =
            match key with
            | One -> One
            | Two -> One
            | Three -> Two
            | Four -> Four
            | Five -> Four
            | Six -> Five
            | Seven -> Seven
            | Eight -> Seven
            | Nine -> Eight
        let stepRight key =
            match key with
            | One -> Two
            | Two -> Three
            | Three -> Three
            | Four -> Five
            | Five -> Six
            | Six -> Six 
            | Seven -> Eight
            | Eight -> Nine
            | Nine -> Nine

        let step key move =
            match move with
            | Right -> stepRight key
            | Left -> stepLeft key
            | Up -> stepUp key
            | Down -> stepDown key

        let follow = List.fold step

    module B =

        type Key = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | A | B | C | D

        let stepUp key =
            match key with 
            | One -> One
            | Two -> Two
            | Three -> One
            | Four -> Four
            | Five -> Five
            | Six -> Two
            | Seven -> Three
            | Eight -> Four
            | Nine -> Nine
            | A -> Six
            | B -> Seven
            | C -> Eight
            | D -> B
        



        let stepDown key =
            match key with
            | One -> Three
            | Two -> Six
            | Three -> Seven
            | Four -> Eight
            | Five -> Five
            | Six -> A
            | Seven -> B
            | Eight -> C
            | Nine -> Nine
            | A -> A
            | B -> D
            | C -> C
            | D -> D
        
        let stepRight key =
            match key with
            | One -> One
            | Two -> Three
            | Three -> Four
            | Four -> Four
            | Five -> Six
            | Six -> Seven
            | Seven -> Eight
            | Eight -> Nine
            | Nine -> Nine
            | A -> B
            | B -> C
            | C -> C
            | D -> D
        
        let stepLeft key =
            match key with
            | One -> One
            | Two -> Two
            | Three -> Two
            | Four -> Three
            | Five -> Five
            | Six -> Five
            | Seven -> Six
            | Eight -> Seven
            | Nine -> Eight
            | A -> A
            | B -> A
            | C -> B
            | D -> D
        
        let step key move =
            match move with
            | Right -> stepRight key
            | Left -> stepLeft key
            | Up -> stepUp key
            | Down -> stepDown key

        let follow = List.fold step

module Parser =

    open AoC.Utils.Helpers
    open Domain

    let asMove c =
        match c with
        | 'R' -> Some Right
        | 'L' -> Some Left
        | 'U' -> Some Up
        | 'D' -> Some Down
        | _ -> None
    let asSteps = String.asChars >> List.choose asMove

    module A =
        open Domain.A

        let asString k =
            match k with
            | One -> "1"
            | Two -> "2"
            | Three -> "3"
            | Four -> "4"
            | Five -> "5"
            | Six -> "6"
            | Seven -> "7"
            | Eight -> "8"
            | Nine -> "9"


    module B =
        open Domain.B

        let asString k =
            match k with
            | One -> "1"
            | Two -> "2"
            | Three -> "3"
            | Four -> "4"
            | Five -> "5"
            | Six -> "6"
            | Seven -> "7"
            | Eight -> "8"
            | Nine -> "9"
            | A -> "A"
            | B -> "B"
            | C -> "C"
            | D -> "D"

module Solver =

    open Domain
    open Parser


    module A =
        open Domain.A
        open Parser.A

        let rec followAll code ( moves : Move list list) =
            let start =
                match code with
                | [] -> Five
                | s::_ -> s
            
            match moves with
            | [] -> List.rev code
            | ms::rest -> followAll ((follow start ms)::code) rest

        let solve = List.map asSteps >> followAll [] >> List.map asString >> String.concat ""

    module B =

        open Domain.B
        open Parser.B

        let rec followAll code ( moves : Move list list) =
            let start =
                match code with
                | [] -> Five
                | s::_ -> s
            
            match moves with
            | [] -> List.rev code
            | ms::rest -> followAll ((follow start ms)::code) rest

        let solve = List.map asSteps >> followAll [] >> List.map asString >> String.concat ""
