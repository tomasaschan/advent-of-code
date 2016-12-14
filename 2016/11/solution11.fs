namespace AoC.Dec11

module Domain =

    type Thing =
    | Generator of string
    | Microchip of string

    let compatible a b =
        match a, b with
        | Microchip c, Generator g -> g = c
        | Generator g, Microchip c -> g = c
        | _ -> false
    
    let dangerous a b =
        match a, b with
        | Microchip c, Generator g -> g <> c
        | Generator g, Microchip c -> g <> c
        | _ -> true
    
    type Floor = Thing list

    let isValidMove floor a =
        let dangers = List.filter (dangerous a) floor

