namespace TLycken.AdventOfCode.Utils

[<AutoOpen>]
module Helpers =
  let optionMap2 f x y =
    match x, y with
    | Some x', Some y' -> Some (f x' y')
    | _ -> None


  let orDefault d = function
    | Some v -> v
    | None -> d