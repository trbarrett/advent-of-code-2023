#load "./Helper.fsx"
open Helper

// Day 7: Camel Cards
// Part 1:
//   * Map the cards to numbers
//   * Categorise them by first sorting them, then doing some rudimentary boolean tests
// Part 2:
//   Same as part 1, except when categorizing the cards try every possible
//   value for the Joker, and take the highest scoring result

type Hand = int * int * int * int * int

type Rank =
    | HighCard of Hand     | OnePair of Hand   | TwoPair of Hand
    | ThreeOfAKind of Hand | FullHouse of Hand | FourOfAKind of Hand
    | FiveOfAKind of Hand

// These following functions only work if the input is sorted
let isFiveOfAKind (a,b,c,d,e) = a = b && b = c && c = d && d = e
let isFourOfAKind (a,b,c,d,e) = (a = b && b = c && c = d) || (b = c && c = d && d = e)
let isFullHouse (a,b,c,d,e) = ((a = b && b = c) && (d = e)) || ((a = b) && (c = d && d = e))
let isThreeOfAKind (a,b,c,d,e) = (a = b && b = c) || (b = c && c = d) || (c = d && d = e)
let isTwoPair (a,b,c,d,e) = (a = b && c = d) || (b = c && d = e) || (a = b && d = e)
let isOnePair (a,b,c,d,e) = a = b || b = c || c = d || d = e

let sortHand (a,b,c,d,e) =
    let [a;b;c;d;e] = List.sort [a;b;c;d;e]
    (a,b,c,d,e)

let categorise hand =
    let sortedHand = sortHand hand
    if isFiveOfAKind sortedHand then FiveOfAKind
    elif isFourOfAKind sortedHand then FourOfAKind
    elif isFullHouse sortedHand then FullHouse
    elif isThreeOfAKind sortedHand then ThreeOfAKind
    elif isTwoPair sortedHand then TwoPair
    elif isOnePair sortedHand then OnePair
    else HighCard

let replaceJokerWith k (a, b, c, d, e) =
    let [a;b;c;d;e] = [a;b;c;d;e] |> List.map (fun x -> if x = 1 then k else x)
    (a,b,c,d,e)

let categoriseWithJoker hand =
    [ for k in 2..13 -> replaceJokerWith k hand ]
    |> List.map categorise
    |> List.map ((|>) hand)
    |> List.sortDescending
    |> List.head

let categoriseNoJoker hand = categorise hand hand

let cardValue c =
    match c with
    | 'T' -> 10 | 'J' -> 11 | 'Q' -> 12 | 'K' -> 13 | 'A' -> 14
    | c -> Char.digitToInt c

let cardValueJokers c =
    match c with
    | 'J' -> 1 | 'T' -> 10 | 'Q' -> 11 | 'K' -> 12 | 'A' -> 13
    | c -> Char.digitToInt c

let calc input cardValue categorise =
    input
    |> List.map (fun [|hand; bid|] ->
        let [a;b;c;d;e] = hand |> Seq.toList |> List.map cardValue
        (a,b,c,d,e), int64 bid)
    |> List.map (fun (h,b) -> categorise h, b)
    |> List.sort
    |> List.indexed
    |> List.sumBy (fun (i, (_, b)) -> int64 (i + 1) * b)

let part1 input = calc input cardValue categoriseNoJoker
    // Part 1 result: 249638405 took: 2,172µs // took 32 mins to write solution

let part2 input = calc input cardValueJokers categoriseWithJoker
    // Part 2 result: 249776650 took: 14ms // took 52 mins to write solution (part1 + part2)

let input =
    Puzzle.readLines "day07.txt" |> Seq.toList
    |> List.map (String.splitStr " ")

Puzzle.warmup part1 part2 input // warm it up for more accurate timings

Puzzle.measurePart1µs part1 input
Puzzle.measurePart2ms part2 input