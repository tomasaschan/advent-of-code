namespace TLycken.AdventOfCode.Utils

open System
module String =
  let join sep = List.reduce (fun a b -> sprintf "%s%s%s" a sep b)

  let joinC sep = List.fold (fun a b -> sprintf "%s%s%c" a sep b) ""

  let split (sep : string) (s : string) = s.Split([|sep|], StringSplitOptions.None) |> List.ofArray

  let asChars (str : string) = str.ToCharArray() |> List.ofArray

