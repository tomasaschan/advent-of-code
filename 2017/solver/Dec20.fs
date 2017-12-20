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
  let acceleration (_,_,(ax,ay,az)) = abs ax + abs ay + abs az
  let solve = List.choose Parse.particle >> List.indexed >> List.minBy (snd >> acceleration) >> fst >> sprintf "%i"

module B =
  let update ((x,y,z),(vx,vy,vz),(ax,ay,az)) =
    let vx', vy', vz' = vx + ax, vy + ay, vz + az
    let x', y', z' = x + vx', y + vy', z + vz'
    ((x',y',z'), (vx',vy', vz'), (ax,ay,az))
  
  let collides ((x,y,z),_,_) ((x',y',z'),_,_) = x = x' && y = y' && z = z'
  let position (p,_,_) = p

  let removeCollisions = List.groupBy position >> List.filter (snd >> List.length >> (=) 1) >> List.collect snd

  let rec tick t tend =
    if t = tend
    then id
    else List.map update >> removeCollisions >> tick (t+1) tend

  let rec tickToSteadyState tstep removed particles =
    match removed with
    | Some 0 -> List.length particles
    | _ ->
      let particles' = tick 0 tstep particles
      let removed' = List.length particles - List.length particles'
      tickToSteadyState tstep (Some removed') particles'

  let solve = List.choose Parse.particle >> tickToSteadyState 10 None >> sprintf "%i"

let solvers = A.solve, B.solve
