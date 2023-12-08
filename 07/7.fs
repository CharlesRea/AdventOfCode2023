module Day7

open System.IO
open Common
open FSharpPlus
open Microsoft.FSharp.Collections

let input = File.ReadLines("./07/input.txt")

type Card =
    | Ace
    | King
    | Queen
    | Jack
    | Number of int

[<StructuredFormatDisplay("{RawCards}")>]
type Hand =
    { Cards: Card list
      RawCards: string
      Bid: int
      Score: int }

let parseCard =
    function
    | "A" -> Ace
    | "K" -> King
    | "Q" -> Queen
    | "J" -> Jack
    | "T" -> Number 10
    | Int x -> Number x
    | x -> failwith $"Invalid card {x}"

let parseHand (scoreHand: Card list -> int) (hand: string) : Hand =
    let split = hand |> String.split [ " " ] |> Seq.toList

    match split with
    | [ cards; Int bid ] ->
        let parsedCards = cards |> Seq.map (string >> parseCard) |> Seq.toList

        { Cards = parsedCards
          RawCards = cards
          Bid = bid
          Score = scoreHand parsedCards }
    | _ -> failwith $"Invalid hand {hand}"

let compareHands (cardScore: Card -> int) (hand1: Hand) (hand2: Hand) : int =
    let rec tieBreak (cards1: Card list) (cards2: Card list) : int =
        match (cards1, cards2) with
        | head1 :: rest1, head2 :: rest2 when head1 = head2 -> tieBreak rest1 rest2
        | head1 :: rest1, head2 :: rest2 -> compare (cardScore head1) (cardScore head2)
        | _ -> 0

    let typeComparison = compare (hand1.Score) (hand2.Score)

    if typeComparison <> 0 then
        typeComparison
    else
        tieBreak hand1.Cards hand2.Cards

module Part1 =
    let cardScore =
        function
        | Ace -> 14
        | King -> 13
        | Queen -> 12
        | Jack -> 11
        | Number x -> x

    let handScore (cards: Card list) : int =
        match cards |> List.countBy id |> List.sortByDescending snd with
        | [ (x, 5) ] -> 6
        | [ (x, 4); (y, 1) ] -> 5
        | [ (x, 3); (y, 2) ] -> 4
        | (x, 3) :: _ -> 3
        | (x, 2) :: (y, 2) :: _ -> 2
        | (x, 2) :: _ -> 1
        | (x, 1) :: _ -> 0
        | x -> failwith $"Unrecognised hand %A{x}"

    let result () =
        let hands =
            input
            |> Seq.map (parseHand handScore)
            |> Seq.sortWith (compareHands cardScore)
            |> Seq.toList

        printSequence "hands" hands |> ignore

        hands |> Seq.mapi (fun i hand -> (i + 1) * hand.Bid) |> Seq.sum

module Part2 =
    let cardScore =
        function
        | Ace -> 14
        | King -> 13
        | Queen -> 12
        | Jack -> 0
        | Number x -> x

    let rec handScore (cards: Card list) : int =
        match cards |> List.countBy id |> List.sortByDescending snd with
        | [ (x, 5) ] -> 6
        | x when cards |> Seq.contains Jack ->
            let nonJacks = cards |> List.except [ Jack ] |> List.distinct

            let optionsToTry =
                cards
                |> List.map (fun card -> if card = Jack then nonJacks else [ card ])
                |> product

            optionsToTry |> Seq.map (handScore) |> Seq.max
        | [ (x, 4); (y, 1) ] -> 5
        | [ (x, 3); (y, 2) ] -> 4
        | (x, 3) :: _ -> 3
        | (x, 2) :: (y, 2) :: _ -> 2
        | (x, 2) :: _ -> 1
        | (x, 1) :: _ -> 0
        | x -> failwith $"Unrecognised hand %A{x}"

    let result () =
        let hands =
            input
            |> Seq.map (parseHand handScore)
            |> Seq.sortWith (compareHands cardScore)
            |> Seq.toList

        printSequence "hands" hands |> ignore

        hands |> Seq.mapi (fun i hand -> (i + 1) * hand.Bid) |> Seq.sum

let solve =
    { PartOne = Part1.result
      PartTwo = Part2.result }
