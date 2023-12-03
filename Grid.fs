module Grid

let private neighbourDiffs =
    Seq.allPairs (seq { -1 .. 1 }) (seq { -1 .. 1 })
    |> Seq.except [(0, 0)]
    |> Seq.toList

let neighbouringCoordinates (x: int, y: int) : (int * int) seq =
    neighbourDiffs
    |> Seq.map (fun (x1, y1) -> (x + x1, y + y1) )
