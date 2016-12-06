namespace AoC.Utils

module Runner =

    let read = fun (filename : string) ->
        if System.IO.File.Exists filename
        then (System.IO.File.ReadLines filename) |> List.ofSeq
        else []

    let private _time fn =
        let watch = System.Diagnostics.Stopwatch.StartNew()
        let answer = fn ()
        watch.Stop ()
        
        answer, watch.ElapsedMilliseconds

    let private _input argv =
        match argv |> List.ofArray with
        | [both] -> Some (read both, read both)
        | a::[b] -> Some (read a, read b)
        | _ -> None

    let private _timedSolve solver input =
        _time (fun _ -> solver input)

    let private _solve argv a b =
        match _input argv with
        | Some(ina, inb) -> _time (fun _ -> a ina), _time (fun _ -> b inb) 
        | None -> _time (fun _ -> a []), _time (fun _ -> b [])

    let private _output problem (solution, time) =
        sprintf "%s: %s (%d ms)" problem solution time

    let solve (argv : string[]) a b =
        let sa, sb = _solve argv a b
        sprintf "%s\n%s" (_output "a" sa) (_output "b" sb)
    
    let private _lift1 (solver : string -> string) =
        (fun input -> 
            match input |> List.ofSeq with
            | [] -> "empty"
            | [x] -> sprintf "%s" (solver x)
            | head :: _ -> "unexpected input - expected just one line")

    let solve1 (argv : string[]) a b = solve argv (_lift1 a) (_lift1 b)
    
    let private _lift0 solver = (fun _ -> solver ())

    let solve0 a b = solve [||] (_lift0 a) (_lift0 b)

    let todo _ = "not implemented"