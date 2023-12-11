module Day11

open System.IO
open Common

let input = File.ReadAllLines("./11/input.txt")
let width = input[0].Length
let height = input.Length

let galaxies =
    input
    |> Seq.mapi (fun y line -> line |> Seq.mapi (fun x symbol -> (x, y), symbol))
    |> Seq.concat
    |> Seq.filter (snd >> ((=) '#'))
    |> Seq.map fst
    |> Seq.toList

let rowsWithGalaxies = galaxies |> Seq.map fst |> Set.ofSeq
let columnsWithGalaxies = galaxies |> Seq.map snd |> Set.ofSeq

let rowsWithoutGalaxies =
    seq { 0 .. width - 1 } |> Seq.filter (fun x -> not (Set.contains x rowsWithGalaxies)) |> Set.ofSeq

let columnsWithoutGalaxies =
    seq { 0 .. height - 1 } |> Seq.filter (fun y -> not (Set.contains y columnsWithGalaxies)) |> Set.ofSeq

let distance (expansion: int64) ((xA, yA),(xB, yB)) =
    let rangeBetween a b =
        (if b > a then seq { a..b } else seq { b..a }) |> Set.ofSeq

    (abs (xB - xA) |> int64)
    + (abs (yB - yA) |> int64)
    + (int64 ((rangeBetween xA xB) |> Set.intersect rowsWithoutGalaxies |> Set.count) * expansion)
    + (int64 ((rangeBetween yA yB) |> Set.intersect columnsWithoutGalaxies |> Set.count) * expansion)

module Part1 =
    let result () =
        combinations galaxies
        |> Seq.map (distance 1)
        |> Seq.sum

module Part2 =
    let result () =
        combinations galaxies
        |> Seq.map (distance 999999L)
        |> Seq.sum

let solve =
    { PartOne = Part1.result
      PartTwo = Part2.result }
