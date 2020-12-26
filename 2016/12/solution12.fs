namespace AoC.Dec12

module Solver =

    open AoC.Utils.Asembunny

    let solve c: string list -> string =
        List.choose Parse.instruction
        >> run (init 0 0 c 0)
        >> (fun m -> m.A)
        >> sprintf "%d"

    module A =

        let solve = solve 0

    module B =

        let solve = solve 1
