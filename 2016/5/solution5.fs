namespace AoC.Dec5

module Domain =

    open AoC.Utils.Helpers
    
    let hash (str : string) = 
        use md5 = System.Security.Cryptography.MD5.Create()
        str 
        |> System.Text.Encoding.UTF8.GetBytes
        |> md5.ComputeHash
        |> Seq.map (fun c -> c.ToString("X2"))
        |> Seq.reduce (+)

    let doorId str i =  sprintf "%s%d" str i

    module A =
        let key str i =
            match (doorId str >> hash >> asChars) i with
            | '0'::'0'::'0'::'0'::'0'::k::_ -> Some k
            | _ -> None

    module B =

        let private validIndex i = 0 <= i && i < 8

        let private charToInt = sprintf "%c" >> parseInt

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

        let key str = (doorId str >> hash >> asChars >> keyChar) >> validKeyChar
            
        let character = fst
        let position = snd


module Solver =

    open Domain
    open AoC.Utils.Helpers

    module A =

        let solve input =
            Seq.initInfinite id
                |> Seq.choose (A.key input)
                |> Seq.take 8
                |> join

    module B =

        open Domain.B

        let solve input =
            Seq.initInfinite id
                |> Seq.choose (key input)
                |> Seq.distinctBy position
                |> Seq.take 8
                |> Seq.sortBy position
                |> Seq.map character
                |> join
