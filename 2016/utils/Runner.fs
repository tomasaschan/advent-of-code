namespace AoC.Utils

module Runner =

    let read = fun (filename : string) ->
        if System.IO.File.Exists filename
        then (System.IO.File.ReadLines filename) |> List.ofSeq
        else []

    let private _solve (argv : string[]) a b =
        match argv |> Array.toList with
        | [input] -> sprintf "a: %s\nb: %s" (a (read input)) (b (read input))
        | ina::[inb] -> sprintf "a: %s\nb: %s" (a (read ina)) (b (read inb))
        | _ ->  sprintf "incorrect input '%s'" (System.String.Join(";", argv))

    let solve (argv : string[]) a b = _solve argv a b
    
    let private _lift1 (solver : string -> string) =
        (fun input -> 
            match input |> List.ofSeq with
            | [] -> "empty"
            | [x] -> sprintf "%s" (solver x)
            | head :: _ -> "unexpected input - expected just one line")

    let solve1 (argv : string[]) a b = _solve argv (_lift1 a) (_lift1 b)
    
    let solve0 a b = sprintf "a: %s\nb: %s" (a ()) (b ())

    let todo _ = "not implemented"