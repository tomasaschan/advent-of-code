namespace TLycken.AdventOfCode.Utils

[<AutoOpen>]
module Helpers =
  let splitString delimiters (str : string)  = str.Split delimiters |> List.ofArray

  let optionMap2 f x y =
    match x, y with
    | Some x', Some y' -> Some (f x' y')
    | _ -> None

