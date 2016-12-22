namespace AoC.Utils

module Hash =

    let md5hasher = System.Security.Cryptography.MD5.Create()

    let md5 (str : string) = 
        str 
        |> System.Text.Encoding.UTF8.GetBytes
        |> md5hasher.ComputeHash
        |> Seq.map (fun c -> c.ToString("X2"))
        |> Seq.reduce (+)