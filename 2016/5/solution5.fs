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

module Solver =

    open Domain
    open AoC.Utils.Helpers

    module A =

        let solve input =
            Seq.unfold (fun state -> Some(state, state+1)) 0
                |> Seq.map (A.key input)
                |> Seq.choose id
                |> Seq.take 8
                |> join
