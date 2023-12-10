module Day3

open System
open System.IO
open System.Text.RegularExpressions
open Common

let input = File.ReadAllLines("./03/input.txt")

let width = input[0].Length
let height = input.Length

let numbers: seq<(int*int) * int> =
    input
    |> Seq.mapi (fun y line -> Regex("(\d+)").Matches line |> Seq.map (fun m -> ((m.Index, y), tryParseInt m.Value |> Option.get)))
    |> Seq.concat
    
let numberLength (value: int): int =
    Math.Log10 value |> Math.Floor |> int

module Part1 =
    let isSymbol (char: char) =
        not (Regex("\d|\.").IsMatch(string char))

    let symbolCoords =
        Seq.allPairs (seq { 0 .. width - 1 }) (seq { 0 .. height - 1 })
        |> Seq.filter (fun (x, y) -> (isSymbol input[y].[x]))
        |> Set.ofSeq
 
    let isPartNumber ((x, y), number: int) =
        let adjacentCoords =
            seq { x .. x + numberLength number }
            |> Seq.collect (fun x -> Grid.neighbouringCoordinatesWithDiagonals (x, y))
            |> Set.ofSeq
            
        adjacentCoords
        |> Seq.exists (fun coords -> Set.contains coords symbolCoords)

    let result () =
        numbers
        |> Seq.filter isPartNumber
        |> Seq.map snd
        |> Seq.sum

module Part2 =
    [<ReferenceEquality>]
    type PartNumber = { Value: int }
    
    let numberCoords =
        numbers
        |> Seq.collect (
            fun ((x, y), value) ->
                let partNumber = { Value  = value }
                seq { x .. x + numberLength value } |> Seq.map (fun x -> (x, y), partNumber))
        |> Map.ofSeq

    let gears =
        Seq.allPairs (seq { 0 .. width - 1 }) (seq { 0 .. height - 1 })
        |> Seq.filter (fun (x, y) -> input[y].[x] = '*')
        |> Seq.map (fun gear -> Grid.neighbouringCoordinatesWithDiagonals gear |> Seq.choose (fun coord -> Map.tryFind coord numberCoords) |> Seq.distinct |> Seq.toArray)
        |> Seq.filter (fun partNumbers -> Array.length partNumbers = 2)
        
    printSequence "Gears" gears |> ignore
    
    let result () =
        gears
        |> Seq.map (fun parts -> parts |> Seq.map (fun { Value = value } -> value))
        |> Seq.sumBy (Seq.reduce (*))
        
let solve =
    { PartOne = Part1.result
      PartTwo = Part2.result }
