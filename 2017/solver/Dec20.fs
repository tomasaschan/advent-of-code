module TLycken.AdventOfCode.Solutions.Dec20
open TLycken.AdventOfCode.Utils

module Parse =
  let vector =
    function
    | Regex.Match "(.+),(.+),(.+)" [x;y;z] -> (Parse.double x, Parse.double y, Parse.double z) |> Option.bind3
    | _ -> None
  let particle =
    function
    | Regex.Match "p=<(.+)>, v=<(.+)>, a=<(.+)>" [p;v;a] -> (vector p, vector v, vector a) |> Option.bind3
    | _ -> None

module A =
  let solve = List.choose Parse.particle >> List.indexed >> List.minBy (snd >> (fun (_, _, (x,y,z)) -> abs x + abs y + abs z)) >> fst >> sprintf "%i"

module B =
  let update ((x,y,z),(vx,vy,vz),(ax,ay,az)) =
    let vx', vy', vz' = vx + ax, vy + ay, vz + az
    let x', y', z' = x + vx', y + vy', z + vz'
    ((x',y',z'), (vx',vy', vz'), (ax,ay,az))
  
  let collides ((x,y,z),_,_) ((x',y',z'),_,_) = x = x' && y = y' && z = z'
  let position ((x,y,z),_,_) = (x,y,z)

  let removeCollisions = List.groupBy position >> List.filter (snd >> List.length >> (=) 1) >> List.collect snd

  let rec tick t tend particles =
    if t = tend
    then List.length particles
    else
      let particles' =
        particles
        |> List.map update
        |> removeCollisions
      tick (t+1) tend particles'

  let solve = List.choose Parse.particle >> tick 0 10000 >> sprintf "%i"

let todo (_ : string list) = "todo"

let solvers = A.solve, B.solve
