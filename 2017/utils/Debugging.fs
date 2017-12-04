namespace TLycken.AdventOfCode.Utils

module Debugging =
  let printAndReturn label x =
    printfn "%s: %A" label x
    x