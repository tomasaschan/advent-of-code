module TLycken.AdventOfCode.Solutions.Dec14

open TLycken.AdventOfCode.Utils

let read = List.head >> (fun s -> (List.init 128 (fun i -> sprintf "%s-%i" s i)))

let toBinary (i : int) = sprintf "%s" <| System.Convert.ToString(i, 2)

let asBinary = asChars >> List.map (sprintf "%c") >> List.choose Parse.hexInt >> List.map toBinary >> List.choose Parse.int >> List.map (sprintf "%04i") >> String.concat ""

let state = read >> List.map (Knot.hash >> asBinary)

module A =
  let solve = state >> List.sumBy (asChars >> List.filter ((=) '1') >> List.length) >> sprintf "%i"

let todo (_ : string list) = "todo"

let solvers = A.solve, todo