module TLycken.AdventOfCode.Tests.List

open Swensen.Unquote
open FsCheck.Xunit
open TLycken.AdventOfCode.Utils
open Xunit


// [<Property>]
// let ``List.insert lengthens list by 1``<'a> (xs : 'a list) (x : 'a) =
//   let n = List.length xs
//   let xs' = List.insert 0 x xs
//   let n' = List.length xs'

//   n =! n'

[<Fact>]
let ``List.insert inserts element at correct position`` () =
  let xs = [0;1;3]
  let x = 2
  let xs' = List.insert 1 x xs

  xs' =! [0;1;2;3]
