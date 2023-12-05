module Day5

open System
open System.IO
open Common
open FSharpPlus

let input = File.ReadAllLines("./05/input.txt")

let parseSeeds (seeds: string) : int64 list =
    seeds |> String.split [ " " ] |> Seq.skip 1 |> Seq.map parseInt64 |> List.ofSeq

type Interval =
    {
    Start: int64
    End: int64
    }

let intersect (x: Interval) (y: Interval): Interval option =
    match (x.Start <= y.End) && (y.Start <= x.End) with
    | true -> Some { Start = Math.Max(x.Start, y.Start); End = Math.Min(x.End, y.End) }
    | false -> None
    
let shiftInterval (x: Interval) (delta: Int64): Interval =
    { Start = x.Start + delta; End = x.End + delta; }
    
let isEmpty (interval: Interval) =
    interval.End < interval.Start

[<StructuredFormatDisplay("{FormattedString}")>]
type MapLine =
    { Source: Interval
      MappingDelta: int64
      Level: int }
    
    member this.Destination: Interval = shiftInterval this.Source this.MappingDelta
    
    member this.FormattedString = $"({this.Source.Start} - {this.Source.End}) => ({this.Source.Start + this.MappingDelta} - {this.Source.End + this.MappingDelta}) - Delta {this.MappingDelta} - Level {this.Level}"

let parseMap (i: int) (mapInput: string seq) : MapLine list =
    let parseInt64Seq (numbers: string) =
        numbers |> String.split [ " " ] |> Seq.map parseInt64

    let parseMapLine (line: string) : MapLine =
        match line |> parseInt64Seq |> Seq.toList with
        | [ destinationStart; sourceStart; range ] ->
            { Source = { Start = sourceStart; End = sourceStart + range - 1L }
              MappingDelta = destinationStart - sourceStart
              Level = i + 1 }
        | _ -> failwith $"Invalid map line {line}"

    mapInput |> Seq.skip 1 |> Seq.map parseMapLine |> Seq.toList

let rawSeeds = parseSeeds input[0]

let maps =
    input[2..]
    |> String.concat "\n"
    |> String.split [ "\n\n" ]
    |> Seq.map ((String.split [ "\n" ]))
    |> Seq.mapi parseMap
    |> Seq.toList
    
let seedLocation (seed: int64) =
    let findNextValue (current: int64) (map: MapLine list) : int64 =
        match
            (map
             |> List.tryFind (fun line -> line.Source.Start <= current && line.Source.End >= current))
        with
        | Some mapLine -> current - mapLine.Source.Start + mapLine.MappingDelta
        | None -> current

    maps |> List.fold findNextValue seed

module Part1 =
    let result () =
        printSequence "Maps" maps |> ignore
        let seedLocations = rawSeeds |> Seq.map seedLocation |> Seq.toList
        printSequence "Seed Locations" seedLocations |> ignore
        seedLocations |> Seq.min

module Part2 =
    type State = MapLine list
    
    let combineLines (next: MapLine) (current: MapLine) : MapLine list =
        match current.Level = next.Level with
        | true ->
            // We've already mapped it - do nothing
            // This is a horrendous hacky mess that could definitely be expressed more cleanly
            [ current ]
        | false -> 
            match intersect current.Destination next.Source with
            | Some intersection ->
                // The ranges overlap. Make the intersection point at the new mapping. Leave rest unchanged.
                
                let lowerLength = (intersection.Start - current.Destination.Start)
                let upperLength = current.Destination.End - intersection.End
                
                let sourceIntersectionStart = current.Source.Start + lowerLength
                let sourceIntersectionEnd = current.Source.End - upperLength
                
                let lower = { current with Source = { Start = current.Source.Start; End = sourceIntersectionStart - 1L } }
                let mid = { Source = {  Start = sourceIntersectionStart; End = sourceIntersectionEnd }; MappingDelta = current.MappingDelta + next.MappingDelta; Level = next.Level }
                let upper = { current with Source = { Start = sourceIntersectionEnd + 1L; End = current.Source.End } }
                
                printf $"\nIntersection found between {current} and {next}.\n Next state: {[lower; mid; upper]}\n"
                
                [ lower; mid; upper ] |> List.filter (fun line -> not (isEmpty line.Source))
            | None ->
                // Intervals don't overlap - so current mapping is unchanged
                [ current ]
        
    let applyMapLine (current: State) (next: MapLine): State =
        current |> List.collect (combineLines next)
    
    let nextState (current: State) (map: MapLine list) =
        let next = map |> List.fold applyMapLine current |> List.sortBy (fun map -> map.Source.Start)
        printf "Applied next map state\n"
        printSequence "Map state" next |> ignore
        printf "\n\n"
        next

    let result () =
        printSequence "Maps" maps |> ignore
        
        let initialState =
            rawSeeds
            |> Seq.chunkBySize 2
            |> Seq.map (fun chunk -> { Source = { Start = chunk[0]; End = chunk[0] + chunk[1] - 1L }; MappingDelta = 0; Level = 0 })
            |> Seq.toList
            |> List.sortBy (fun map -> map.Source.Start)
            
        printSequence "Initial map state" initialState |> ignore

        let finalState = maps |> List.fold nextState initialState
        
        finalState |> Seq.map (fun map -> map.Source.Start + map.MappingDelta) |> Seq.min

let solve =
    { PartOne = Part1.result
      PartTwo = Part2.result }
