module TLycken.AdventOfCode.Tests.String

open TLycken.AdventOfCode.Utils
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Joins strings to string with separator`` () =
  let ss = ["foo";"bar";"baz"]
  String.join " - " ss =! "foo - bar - baz"

[<Fact>]
let ``Splits strings by separator`` () =
  let s = "foo - bar - baz"
  String.split " - " s =! ["foo";"bar";"baz"]
