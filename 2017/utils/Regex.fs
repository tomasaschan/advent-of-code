module TLycken.AdventOfCode.Utils.Regex

open System.Text.RegularExpressions

let (|Match|_|) regex str =
  let m = Regex(regex).Match(str)
  if m.Success
  then
    [for x in m.Groups -> x.Captures]
    |> List.tail
    |> List.collect (fun cs -> [for c in cs -> c.Value])
    |> Some
  else None

let (|Matches|_|) regex str =
  let ms = Regex(regex).Matches(str)
  if ms.Count > 0
  then
    let cs = [for m in ms -> m.Captures]
    let vs = cs |> List.collect (fun c -> [for v in c -> v.Value])
    Some vs
  else None
