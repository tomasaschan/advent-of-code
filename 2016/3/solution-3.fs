namespace AoC.Dec3



module Domain = 

    type Triangle = { a : int; b : int; c : int }

    let isValid triangle =
        let a, b, c = triangle.a, triangle.b, triangle.c
        a + b > c && a + c > b && a + c > b

module Parse =

    open AoC.Utils.Helpers
    open Domain

    let toTriangle ints =
        match ints with
        | [a;b;c] -> Some { a = a; b = b; c = c }
        | _ -> None

    let ints input =
        input
            |> split " "
            |> List.filter (fun s -> s.Length > 0)
            |> List.map parseInt
            |> List.choose id
        
    
    let triangle input =
        input
        |> List.sort
        |> toTriangle

module Solvers =

    module A = 

        open Domain

        let solve input =
            let answer = 
                input
                |> List.map (Parse.ints >> Parse.triangle)
                |> List.choose id
                |> List.filter isValid
                |> List.length

            sprintf "%d" answer

    module B =

        open Domain

        let pivot chunk =
            match chunk with
            | [[a;b;c];[d;e;f];[g;h;i]] -> [[a;d;g];[b;e;h];[c;f;i]]
            | other -> other

        let solve input =
            let answer =
                input
                |> List.map Parse.ints
                |> List.chunkBySize 3
                |> List.map pivot
                |> List.collect id
                |> List.map Parse.triangle
                |> List.choose id
                |> List.filter isValid
                |> List.length

            sprintf "%d" answer