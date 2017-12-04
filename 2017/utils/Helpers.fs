namespace TLycken.AdventOfCode.Utils

[<AutoOpen>]
module Helpers =
  let splitString delimiters (str : string)  = str.Split delimiters |> List.ofArray

  let asChars (str : string) = str.ToCharArray() |> List.ofArray

  let asString = List.map (sprintf "%c") >> String.concat ""

  let optionMap2 f x y =
    match x, y with
    | Some x', Some y' -> Some (f x' y')
    | _ -> None


  let orDefault d = function
    | Some v -> v
    | None -> d