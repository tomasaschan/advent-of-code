namespace Dec1

module A =

    let solve = fun (input : string seq ) -> 
        match input |> Seq.toList with
        | [] -> "empty"
        | [x] -> "got one line"
        | head :: _ -> "got many lines"
