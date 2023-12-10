module Grid

let private neighbourDiffsWithDiagonals =
    Seq.allPairs (seq { -1 .. 1 }) (seq { -1 .. 1 })
    |> Seq.except [(0, 0)]
    |> Seq.toList

let neighbouringCoordinatesWithDiagonals (x: int, y: int) : (int * int) seq =
    neighbourDiffsWithDiagonals
    |> Seq.map (fun (x1, y1) -> (x + x1, y + y1) )


let private neighbourDiffsWithoutDiagonals =
    [(1, 0); (0, 1); (-1, 0); (0, -1)]

let neighbouringCoordinatesWithoutDiagonals (x: int, y: int) : (int * int) seq =
    neighbourDiffsWithoutDiagonals
    |> Seq.map (fun (x1, y1) -> (x + x1, y + y1) )
