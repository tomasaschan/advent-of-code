namespace AoC.Dec19

module Domain =

    open AoC.Utils.Helpers

    let helpers count = Seq.init count id

    let second<'a> = Seq.skip 1 >> Seq.tryHead

    let third<'a> = Seq.skip 2 >> Seq.tryHead
    let exceptSecond s =
        match Seq.tryHead s with
        | Some h -> (Seq.skipOrEmpty 2 >> Seq.prepend h) s
        | None -> s

    let rec play helpers =
        match (Seq.skip 1 >> Seq.tryHead) helpers with
        | Some h -> play (Seq.append [h] (Seq.skip 2 helpers))
        | _ -> None


module Solver =

    open Domain

    module A =

        let solve _  = "todo" //  helpers >> play
