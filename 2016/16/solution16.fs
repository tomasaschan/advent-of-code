namespace AoC.Dec16

module Domain =

    open AoC.Utils.Helpers

    let private invert = function
        | '0' -> '1'
        | '1' -> '0'
        | x -> x

    let private prependWith c cs = c::cs

    let private makeSecondHalf = List.rev >> List.map invert >> prependWith '0'

    let private extendOnce data = List.append data (makeSecondHalf data)

    let rec extendTo len data =
        if List.length data >= len
        then List.take len data
        else extendTo len (extendOnce data)

    let private hashSingle = function
        | [a;b] -> if a = b then '1' else '0'
        | x -> 'x'
    
    let private hashOnce : char list -> char list = List.chunkBySize 2 >> List.map hashSingle

    let rec hash data =
        let hashed = hashOnce data
        match List.length hashed with
        | Int.Odd i -> hashed
        | _ -> hash hashed

module Solver =

    open AoC.Utils.Helpers
    open Domain

    let solve len = String.asChars >> extendTo len >> hash >> String.join


    module A = 

        let solve = solve 272
        
    module B =
        let solve = solve 35651584
