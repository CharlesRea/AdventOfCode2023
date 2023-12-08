module Day8

open System
open System.IO
open System.Text.RegularExpressions
open Common
open FSharpx.Collections
open Microsoft.FSharp.Collections

let input = File.ReadAllLines("./08/input.txt")

type Node = { Id: string; Left: string; Right: string; }

let parseNode (node: string): Node =
    match node with
    | ParseRegex "(.*) = \((.*), (.*)\)" [ id; left; right; ] -> { Id = id; Left = left; Right = right; }
    | node -> failwith $"Invalid node input {node}"

type Direction = Right | Left

let parseDirection (direction: char): Direction =
    match direction with
    | 'L' -> Left
    | 'R' -> Right
    | _ -> failwith $"Invalid direction {direction}"

let directions = input[0] |> Seq.map parseDirection |> Seq.toArray

let nodes = input[2..] |> Seq.map parseNode |> Seq.map (fun node -> node.Id, node) |> Map.ofSeq

let nextLocation (direction: Direction) (location: string) =
    match direction with
    | Left -> nodes[location].Left
    | Right -> nodes[location].Right

module Part1 =
    let result () =
        let rec solve (location: string) (count: int): int =
            match (nextLocation directions[count % directions.Length] location) with
            | "ZZZ" -> count + 1
            | nextLocation ->
                solve nextLocation (count + 1)

        solve "AAA" 0

module Part2 =
    let result () =
        let rec solve (location: string) (count: int): int =
            match (nextLocation directions[count % directions.Length] location) with
            | location when location[2] = 'Z' -> count + 1
            | nextLocation ->
                solve nextLocation (count + 1)
               
        // This relies on the fact that (in my data at least), the first end location found from each
        // start point is reached on a multiple of directions.Length, and hence we end up with each
        // path on a loop with the same phase; and we can take the LCM to find the final solution.
        // It's a shame as as the question is posed, this could not be the case and we'd need to account
        // for loops being offset with different phases. So this feels a bit hacky, but is the nature
        // of AoC.
        let startingNodes = nodes |> Map.keys |> Seq.filter (fun node -> node[2] = 'A') |> Seq.toList
        let solutions = startingNodes |> Seq.map (fun start -> solve start 0) |> Seq.toList
        printSequence "Solutions" solutions |> ignore
        solutions |> Seq.map int64 |> Seq.reduce lcm
    
let solve =
    { PartOne = Part1.result
      PartTwo = Part2.result }
