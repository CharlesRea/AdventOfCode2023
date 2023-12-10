module Day10

open System
open System.IO
open System.Text.RegularExpressions
open Common
open FSharpx.Collections.Tagged
open Microsoft.FSharp.Collections
open BlackFox.ColoredPrintf

let input = File.ReadAllLines("./10/input.txt")
let width = input[0].Length
let height = input.Length

let startLocation =
    Seq.allPairs (seq { 0..width-1 }) (seq { 0..height-1 })
    |> Seq.find(fun (x, y) -> input[y][x] = 'S')

type Direction = North | East | South | West

type Pipe = { Connecting: Direction list; Location: int * int; Symbol: char }

let followDirection (x, y) (direction: Direction) =
    match direction with
    | North -> (x, y-1)
    | East -> (x+1, y)
    | South -> (x, y+1)
    | West -> (x-1, y)
        
let connectingPipes (pipes: Map<int*int, Pipe>) ({ Location = (x, y); Connecting = connecting }: Pipe): Pipe list =
    connecting
    |> List.map (followDirection (x, y))
    |> List.choose (fun location -> Map.tryFind location pipes)

let parsePipe (pipe: char) x y: Pipe option =
    match pipe with
    | '|' -> Some { Connecting = [North; South]; Location = (x, y); Symbol = pipe }
    | '-' -> Some { Connecting =  [East; West]; Location = (x, y); Symbol = pipe }
    | 'L' -> Some { Connecting =  [North; East]; Location = (x, y); Symbol = pipe }
    | 'J' -> Some { Connecting =  [North; West]; Location = (x, y); Symbol = pipe }
    | '7' -> Some { Connecting =  [South; West]; Location = (x, y); Symbol = pipe }
    | 'F' -> Some { Connecting =  [South; East]; Location = (x, y); Symbol = pipe }
    | '.'  -> None
    | 'S' ->
        Some { Connecting = []; Location = (x, y); Symbol = 'J' } // TODO: hardcoding the symbol currently
    | _ -> failwith $"Invalid pipe {pipe}"

let pipes =
    let parsedPipes =
        input
        |> Seq.mapi (fun y line -> line |> Seq.mapi (fun x pipe -> parsePipe pipe x y))
        |> Seq.concat
        |> Seq.choose id
        |> Seq.map (fun pipe -> (pipe.Location, pipe))
        |> Map.ofSeq

    let startPipe = Map.find startLocation parsedPipes
    
    let connectionsToStart =
        [North; East; South; West]
        |> List.map (fun direction ->
            direction,
            followDirection startLocation direction
            |> (fun location -> Map.tryFind location parsedPipes)
            |> Option.map (fun pipe -> connectingPipes parsedPipes pipe |> List.contains startPipe)
            |> Option.defaultValue false
        )
        |> List.filter snd
        |> List.map fst

    parsedPipes |> Map.add startLocation  ({ startPipe with Connecting = connectionsToStart })

let pipesInLoop: Map<Pipe, int> =
    let rec solve (visited: Map<Pipe, int>) (lastVisited: Pipe list) (lastDistance: int): Map<Pipe, int> =
        let next =
            lastVisited
            |> Seq.collect (connectingPipes pipes)
            |> Seq.distinct
            |> Seq.filter (fun pipe -> not (Map.containsKey pipe visited))
            |> Seq.toList

        match next with
        | [] -> visited
        | next ->
            let nextVisited = next |> Seq.fold (fun visited pipe -> visited |> Map.add pipe (lastDistance + 1)) visited
            solve (nextVisited) (next) (lastDistance + 1)
    
    let startPipe = pipes |> Map.find startLocation
    solve (Map.empty |> Map.add startPipe 0) [startPipe] 0
    
let pipesInLoopByLocation = pipesInLoop |> Map.keys |> Seq.map (fun pipe -> pipe.Location, pipe) |> Map.ofSeq

module Part1 =   
    let result () =
        pipesInLoop |> Map.values |> Seq.max

module Part2 =
    type PipeDirection = Left | Right

    let isPointInLoop (pointX, pointY): bool =
        // Project a ray from the point to the outside of the grid (arbitrarily picked directly downwards).
        // If ray intersects the loop an even number of times, the point is in the loop.
        // If the ray intersects a pipe heading directly downwards, we need to track whether the pipe
        // ever actually crosses the ray.
        // This code could do with some tidying up, could probably reduce the number of cases checked.
        //
        // Could have solved this with the stdlib but wanted to do it by hand... See https://learn.microsoft.com/en-us/dotnet/api/system.drawing.drawing2d.graphicspath.isvisible?view=dotnet-plat-ext-8.0#system-drawing-drawing2d-graphicspath-isvisible(system-int32-system-int32) 
        let rec solve (currentY: int) (currentCount: int) (lastDirection: PipeDirection option): bool =
            match currentY + 1 with
            | nextY when nextY = height ->  (currentCount % 2 <> 0)
            | nextY ->
                match pipesInLoopByLocation |> Map.tryFind (pointX, nextY) with
                | Some pipe ->
                    match pipe.Symbol with
                    | '|' -> solve nextY (currentCount) lastDirection
                    | '-' -> solve nextY (currentCount + 1) None
                    | 'L' ->
                        match lastDirection with
                        | Some Left -> solve nextY (currentCount + 1) None
                        | Some Right -> solve nextY (currentCount) None
                        | None -> failwith $"Encountered L with no lastDirection at {pointX}, {pointY}, {currentY}"
                    | 'J' ->
                        match lastDirection with
                        | Some Left -> solve nextY (currentCount) None
                        | Some Right -> solve nextY (currentCount + 1) None
                        | None -> failwith $"Encountered J with no lastDirection at {pointX}, {pointY}, {currentY}"
                    | '7' ->
                        match lastDirection with
                        | None -> solve nextY (currentCount) (Some Left)
                        | Some direction -> failwith $"Encountered 7 with a lastDirection of {direction} at {pointX}, {pointY}, {currentY}"
                    | 'F' ->
                        match lastDirection with
                        | None -> solve nextY (currentCount) (Some Right)
                        | Some direction -> failwith $"Encountered F with a lastDirection of {direction} at point ({pointX}, {nextY}) for start point ({pointX}, {pointY})"
                    | x -> failwith $"Invalid pipe symbol {x}"
                | None ->
                    solve nextY currentCount lastDirection
            
        solve pointY 0 None

    let result () =
        let insideLoop =
            Seq.allPairs (seq { 0..width-1 }) (seq { 0..height-1 })
            |> Seq.filter(fun location -> not (Map.containsKey location pipesInLoopByLocation))
            |> Seq.filter(isPointInLoop)
            |> Set.ofSeq

        printf "\n\nFINAL RESULT\n"
        (seq { 0..height - 1 }) |> Seq.iter (fun y ->
            (seq { 0..width - 1 }) |> Seq.iter (fun x ->
                if (insideLoop |> Set.contains (x, y)) then colorprintf "$red[%c]" input.[y].[x] else if (pipesInLoopByLocation |> Map.containsKey (x, y)) then colorprintf "$green[%c]" input.[y].[x] else printf "%c" input.[y].[x] 
            )
            printf "\n"
        )
        
        Set.count insideLoop

let solve =
    { PartOne = Part1.result
      PartTwo = Part2.result }
