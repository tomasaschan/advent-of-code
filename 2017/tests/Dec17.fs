module TLycken.AdventOfCode.Tests.Dec17

open Xunit
open Swensen.Unquote

open TLycken.AdventOfCode.Solutions.Dec17

[<Fact>]
let ``Evolves one step correctly`` () =
  let init, i = [0], 0

  A.evolve init i 3 1 =! ([0;1], 1)
  A.evolve [0;1] 1 3 2 =! ([0;2;1], 1)
  A.evolve [0;2;1] 1 3 3 =! ([0;2;3;1], 2)

[<Fact>]
let ``Solves example A correctly`` () =
  let answer = A.evolveAll [0] 0 3 1 9

  answer =! 5
