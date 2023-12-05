module Day1

open System.IO
open System.Text.RegularExpressions
open Common
open FSharpPlus

let input = File.ReadLines("./01/input.txt")

let calibrate (getDigits: string -> int array): int =
    input
    |> Seq.map getDigits
    |> Seq.map (fun line -> Array.head line * 10 + Array.last line)
    |> Seq.sum

let partOne () =
    calibrate (Seq.map (string >> tryParseInt) >> Seq.choose id >> Seq.toArray)

let digitValue =
    function
    | "one" -> 1
    | "two" -> 2
    | "three" -> 3
    | "four" -> 4
    | "five" -> 5
    | "six" -> 6
    | "seven" -> 7
    | "eight" -> 8
    | "nine" -> 9
    | Int x -> x
    | x -> failwith $"Invalid digit string {x}"
    
let findDigits (line: string): int array =
    Regex("(?=(\d|one|two|three|four|five|six|seven|eight|nine))").Matches(line)
    |> Seq.map (fun x -> digitValue (x.Groups |> Seq.last |> fun x -> x.Value))
    |> Seq.toArray
    
let partTwo () =
    calibrate findDigits
    
let solve = { PartOne = partOne; PartTwo = partTwo }
