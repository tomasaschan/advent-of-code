namespace AoC.Dec3



module Domain = 

    let isValid triangle =
        let a, b, c = triangle
        a + b > c && a + c > b && a + c > b

module Parse =

    open AoC.Utils
    open Domain

    let private _triangle ints =
        match ints with
        | [a;b;c] -> Some(a, b, c)
        | _ -> None

    let removeEmpty = List.filter (fun (s : string) -> s.Length > 0)
    let parseInts = List.choose Int.parse
    let ints = (String.split " ") >> removeEmpty >> parseInts
    let triangle = List.sort >> _triangle

module Solvers =

    module A = 

        open Domain

        let solve = (List.choose (Parse.ints >> Parse.triangle)) >> List.filter isValid >> List.length >> sprintf "%d"

    module B =

        open Domain

        let pivot chunk =
            match chunk with
            | [[a;b;c];[d;e;f];[g;h;i]] -> [[a;d;g];[b;e;h];[c;f;i]]
            | other -> other

        let solve = List.map Parse.ints >> List.chunkBySize 3 >> List.collect pivot >> List.choose Parse.triangle >> List.filter isValid >> List.length >> sprintf "%d"
