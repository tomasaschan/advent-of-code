namespace TLycken.AdventOfCode.Utils

[<AutoOpen>]
module Helpers =
  let splitString delimiters (str : string)  = str.Split delimiters |> List.ofArray
