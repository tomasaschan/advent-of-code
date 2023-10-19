namespace AoC.Dec5

module Domain =

    open AoC.Utils

    let doorId str i =  sprintf "%s%d" str i

    module A =
        let key str i =
            match (doorId str >> Hash.md5 >> String.asChars) i with
            | '0'::'0'::'0'::'0'::'0'::k::_ -> Some k
            | _ -> None

    module B =

        let private validIndex i = 0 <= i && i < 8

        let private charToInt = sprintf "%c" >> Int.parse
        let keyChar chars =
            match chars with
            | '0'::'0'::'0'::'0'::'0'::p::c::_ -> Some (c, charToInt p)
            | _ -> None

        let validKeyChar kc =
            match kc with
            | Some (c, Some p) ->
                if validIndex p
                then Some (c, p)
                else None
            | _ -> None

        let key str = (doorId str >> Hash.md5 >> String.asChars >> keyChar) >> validKeyChar
            
        let character = fst
        let position = snd


module Solver =

    open Domain
    open AoC.Utils

    module A =
        
        open Domain.A

        let solve input =
            Seq.initInfinite id
                |> Seq.choose ((List.head >> key) input)
                |> Seq.take 8
                |> String.join

    module B =

        open Domain.B

        let solve input =
            Seq.initInfinite id
                |> Seq.choose ((List.head >> key) input)
                |> Seq.distinctBy position
                |> Seq.take 8
                |> Seq.sortBy position
                |> Seq.map character
                |> String.join
