module Day6

open System
open System.IO
open System.Text.RegularExpressions
open Common

type Race = { Duration: int64; Record: int64; }

// ax^2 + bx + c = 0
let solveQuadratic (a: double) (b: double) (c: double): double * double =
    let discriminant = b ** 2 - 4.0 * a * c
    (-b - sqrt discriminant) / (2.0 * a), (-b + sqrt discriminant) / (2.0 * a)

let solveRace (race: Race): int64 =
    // Quadratic of form x^2 - time * x + record >= 0
    let root1, root2 = solveQuadratic 1 (double -race.Duration) (double race.Record)
    let lowerBound = if (root1 % 1.0 = 0) then (int64 root1) + 1L else Math.Ceiling root1 |> int64
    let upperBound = if (root2 % 1.0 = 0) then (int64 root2) - 1L else Math.Floor root2 |> int64
    
    printf $"Solution for race {race}: ({root1}, {root2}) - ({lowerBound}, {upperBound})\n\n"
    upperBound - lowerBound + 1L

module Part1 =
    let testRaces = [
        { Duration = 7; Record = 9  };
        { Duration = 15; Record = 40  };
        { Duration = 30; Record = 200  };
    ]

    let races = [
        { Duration = 40; Record = 277  };
        { Duration = 82; Record = 1338 };
        { Duration = 91; Record = 1349  };
        { Duration = 66; Record = 1063  };
    ]
    
    let result () =
        races |> Seq.map solveRace |> Seq.reduce (*)

module Part2 =
    let testRace = { Duration = 71530; Record = 940200 };
    let race = { Duration = 40829166; Record = 277133813491063L };
    
    let result () = solveRace race

let solve =
    { PartOne = Part1.result
      PartTwo = Part2.result }
