module Day10

open System
open System.IO
open System.Text.RegularExpressions
open Common

let input = File.ReadAllLines("./10/input.txt")
let width = input[0].Length
let height = input.Length

type Direction = North | East | South | West

type Pipe = { Connecting: Direction list; Location: int * int }

let parsePipeDirections (pipe: char): Direction list =
    match pipe with
    | '|' -> [North; South]
    | '-' ->  [East; West]
    | 'L' ->  [North; East]
    | 'J' ->  [North; West]
    | '7' ->  [South; West]
    | 'F' ->  [South; East]
    | '.'  -> []
    | 'S' -> []
    | _ -> failwith $"Invalid pipe {pipe}"
    
let pipes =
    input
    |> Seq.mapi (fun y line -> line |> Seq.mapi (fun x pipe -> (x, y), { Connecting = parsePipeDirections pipe; Location = (x, y) }))
    |> Seq.concat
    |> Map.ofSeq

let startLocation = Seq.allPairs (seq { 0..width-1 }) (seq { 0..height-1 }) |> Seq.find(fun (x, y) -> input[y][x] = 'S')
let startPipe = Map.find startLocation pipes

let connectingPipes ({ Location = (x, y); Connecting = connecting }: Pipe): Pipe list =
    let coordsForDirection (direction: Direction) =
        match direction with
        | North -> (x, y-1)
        | East -> (x+1, y)
        | South -> (x, y+1)
        | West -> (x-1, y)
    
    connecting
    |> List.map coordsForDirection
    |> List.choose (fun location -> Map.tryFind location pipes)
    
let startConnections =
    Grid.neighbouringCoordinatesWithoutDiagonals (startPipe.Location)
    |> Seq.choose (fun coord -> pipes |> Map.tryFind coord)
    |> Seq.filter (fun pipe -> connectingPipes pipe |> List.contains startPipe)
    |> Seq.toList
    
printf $"Start pipe at {startPipe.Location}\n"
printSequence "Start connections" startConnections |> ignore

module Part1 =   
    let rec solve (visited: Map<Pipe, int>) (lastVisited: Pipe list) (lastDistance: int): Map<Pipe, int> =
        let next =
            lastVisited
            |> Seq.collect connectingPipes
            |> Seq.distinct
            |> Seq.filter (fun pipe -> not (Map.containsKey pipe visited))
            |> Seq.toList
            
        printf $"Finding pipes for distance {lastDistance}\n"
        printSequence "Next to visit" next |> ignore
            
        match next with
        | [] -> visited
        | next ->
            let nextVisited = next |> Seq.fold (fun visited pipe -> visited |> Map.add pipe (lastDistance + 1)) visited
            solve (nextVisited) (next) (lastDistance + 1)
    
    let result () =
        let finalState = solve (startConnections |> Seq.map (fun pipe -> pipe, 1) |> Map.ofSeq) startConnections 1
        
        printSequence "Final state" finalState |> ignore
        finalState |> Map.values |> Seq.max

module Part2 =
    let result () = 0

let solve =
    { PartOne = Part1.result
      PartTwo = Part2.result }
