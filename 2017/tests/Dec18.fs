module TLycken.AdventOfCode.Tests.Dec18

open Xunit
open Swensen.Unquote
open TLycken.AdventOfCode.Solutions.Dec18
open A

[<Fact>]
let ``Parses sample instructions`` =
  let input = [
    "set a 1"
    "add a 2"
    "mul a a"
    "mod a 5"
    "snd a"
    "set a 0"
    "rcv a"
    "jgz a -1"
    "set a 1"
    "jgz a -2"
  ]

  let instructions = List.choose Parse.instruction input

  instructions =! [
    Set ('a', Number 1L)
    Add ('a', Number 2L)
    Mul ('a', Register 'a')
    Mod ('a', Number 5L)
    Sound (Register 'a')
    Set ('a', Number 0L)
    Recover (Register 'a')
    Jump (Register 'a', Number -1L)
    Set ('a', Number 1L)
    Jump (Register 'a', Number -2L)
  ]