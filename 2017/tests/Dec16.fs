module TLycken.AdventOfCode.Tests.Dec16

open TLycken.AdventOfCode.Solutions.Dec16
open Swensen.Unquote
open Xunit

[<Fact>]
let ``Parses spin`` () =
    Parse.instruction "s3" =! Some (Spin 3)
    Parse.instruction "s150" =! Some (Spin 150)

[<Fact>]
let ``Parses exchange`` () =
    Parse.instruction "x15/2" =! Some (Exchange (15, 2))
    Parse.instruction "x0/99" =! Some (Exchange (0, 99))

[<Fact>]
let ``Parses partner`` () =
    Parse.instruction "pa/e" =! Some (Partner ('a', 'e'))
    Parse.instruction "pp/f" =! Some (Partner ('p', 'f'))

[<Fact>]
let ``Correctly applies spin`` () =
    let state = ['a';'b';'c';'d';'e']
    applySpin 1 state =! ['e';'a';'b';'c';'d']

[<Fact>]
let ``Correctly applies exchange`` () =
    let state = ['e';'a';'b';'c';'d']
    applyExchange 3 4 state =! ['e';'a';'b';'d';'c']

[<Fact>]
let ``Correctly applies partner`` () =
    let state = ['e';'a';'b';'d';'c']
    applyPartner 'e' 'b' state =! ['b';'a';'e';'d';'c']

[<Fact>]
let ``Initializes state correctly`` () =
    initialState 5 =! ['a';'b';'c';'d';'e']

[<Fact>]
let ``Correctly solves example A`` () =
    let input = ["s1,x3/4,pe/b"]
    let init = initialState 5

    let answer = A.solve init input

    answer =! "baedc"

[<Fact>]
let ``Finds cycle length`` () =
    let instructions = Parse.instructions "s1,s1,s1"
    let init = ['a';'b';'c';'d';'e']

    cycleLength 0 init init instructions =! 5

[<Fact>]
let ``Correctly solves example B`` () =
    let input = ["s1,x3/4,pe/b"]
    let init = initialState 5

    let answer = B.solve init 2 input

    answer =! "ceadb"