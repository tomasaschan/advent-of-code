namespace AoC.Utils

module List =
    let enumerate<'a> : List<'a> -> List<int * 'a> =
        Seq.zip (Seq.initInfinite id) >> List.ofSeq
