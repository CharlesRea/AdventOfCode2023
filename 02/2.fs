module Day2

open System.IO
open System.Text.RegularExpressions
open Common
open FSharpPlus

let input = File.ReadLines("./02/input.txt")

type Colour =
    | Blue
    | Red
    | Green
    
let (|Colour|_|) =
    function
    | "blue" -> Some Blue
    | "red" -> Some Red
    | "green" -> Some Green
    | _ -> None
    
let parseDraw (draw): Colour * int =
    match draw with
    | ParseRegex "(\d+) (\w+)" [ Int count; Colour colour ] -> (colour, count)
    | x -> failwith $"Invalid draw {x}"

// Part One
let isValidColourDraw (draw): bool =
    match parseDraw draw with
    | (Blue, count) -> count <= 14
    | (Red, count) -> count <= 12
    | (Green, count) -> count <= 13
    
let isValidDraw (draw): bool =
    draw |> String.split [","] |> Seq.forall isValidColourDraw
    
let isValidGame =
    function
    | ParseRegex "Game \d+: (.*)" [ draws ] ->
        draws |> String.split [ ";" ] |> Seq.forall isValidDraw
    | game -> failwith $"Invalid game {game}"    

let partOne () =
    input
    |> Seq.mapi (fun i game -> (i + 1, isValidGame game))
    |> Seq.filter snd
    |> Seq.sumBy fst

// Part Two
type MinimumCubes = Map<Colour, int>

let minimumCubesForGame (game: string): MinimumCubes =
    game
    |> String.split [", "; ";"]
    |> Seq.map parseDraw
    |> Seq.fold (fun minimum (colour, cubes) ->
        minimum |> Map.add colour (max minimum[colour] cubes)
    ) (Map [ (Blue, 0); (Red, 0); (Green, 0) ])

let partTwo () =
    input
    |> Seq.map minimumCubesForGame
    |> Seq.map (Map.values >> Seq.reduce (*))
    |> Seq.sum
    
let solve = { PartOne = partOne; PartTwo = partTwo }
