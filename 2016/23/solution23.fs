namespace AoC.Dec23

module Solver =
    open AoC.Utils

    let run mul a =
        List.choose (Asembunny.parse mul)
        >> Asembunny.init a 0 0 0
        >> Asembunny.run
        >> (fun m -> m.A)
        >> string


    module A =
        let solve = run true 7

    module B =
        let solve = run true 12
