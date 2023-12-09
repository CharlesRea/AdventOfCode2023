module Day9

open System.IO
open Common
open FSharpPlus

let input =
    File.ReadLines("./09/input.txt")
    |> Seq.map (fun line -> line |> String.split [" "] |> Seq.map parseInt |> Seq.toArray)

module Part1 =
    let rec solve (values: int array): int =
        match (values |> Seq.forall ((=) (Array.head values))) with
        | true -> Array.head values
        | false ->
            let differences = values |> Seq.pairwise |> Seq.map (fun (x, y) -> y - x) |> Seq.toArray
            (Array.last values) + solve differences

    let result () =
        input |> Seq.map solve |> Seq.sum

module Part2 =
    let rec solve (values: int array): int =
        match (values |> Seq.forall ((=) (Array.head values))) with
        | true -> Array.head values
        | false ->
            let differences = values |> Seq.pairwise |> Seq.map (fun (x, y) -> y - x) |> Seq.toArray
            (Array.head values) - solve differences

    let result () =
        input |> Seq.map solve |> Seq.sum

let solve =
    { PartOne = Part1.result
      PartTwo = Part2.result }
