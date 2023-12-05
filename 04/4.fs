module Day4

open System
open System.IO
open Common
open FSharpPlus

let input = File.ReadLines("./04/input.txt")

type Card =
    { CardNumber: int; Winning: int Set; Numbers: int seq }

    member this.WinningNumbers : int =
        this.Numbers
        |> Seq.filter (fun x -> Set.contains x this.Winning)
        |> Seq.length

let parseNumbers (numbers: string) : int seq =
    numbers
    |> String.split [ " " ]
    |> Seq.map String.trimWhiteSpaces
    |> Seq.choose tryParseInt

let parseCard (card: string): Card =
    match card with
    | ParseRegex "Card\s+(\d+):(.*) \| (.*)" [ Int cardNumber; winning; numbers ] ->
        { CardNumber = cardNumber
          Winning = parseNumbers winning |> Set.ofSeq
          Numbers = parseNumbers numbers }
    | _ -> failwith $"Invalid card {card}"

let cards = input |> Seq.map parseCard

module Part1 =
    let cardScore (card: Card): int =
        2f ** ((float32 card.WinningNumbers) - 1f) |> int
    
    let result () =
        cards
        |> Seq.sumBy cardScore

module Part2 =
    let cards = cards |> Seq.toArray

    let cardsCount: Map<int, int> =
        cards
        |> Seq.map (fun card -> card.CardNumber, 1)
        |> Map.ofSeq

    let updateCounts (counts:  Map<int, int>) (card: Card): Map<int, int> =
        let currentCardCount = counts[card.CardNumber]
        seq { card.CardNumber + 1 .. card.CardNumber + card.WinningNumbers }
        |> Seq.fold (fun counts cardNumber -> counts |> Map.change cardNumber (Option.map ((+) currentCardCount))) counts

    let finalCounts: Map<int, int> =
        cards
        |> Seq.fold updateCounts cardsCount

    let result () =
        finalCounts
        |> Map.values
        |> Seq.sum

let solve =
    { PartOne = Part1.result
      PartTwo = Part2.result }
