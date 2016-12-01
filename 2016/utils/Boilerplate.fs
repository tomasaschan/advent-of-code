namespace AoC.Utils

module Runner =

    open AoC.Utils.IO

    let solve argv a b =
        let inputfiles =
            match argv |> Array.toList with
            | ina :: [inb] -> Some (ina,inb)
            | _ ->  None
    
        let output = 
            match inputfiles with
            | Some (ina,inb) -> sprintf "%s\n%s" (a (read ina)) (b (read inb))
            | None -> "incorrect input"

        output

    let todo _ = "not implemented"