#load "./Helper.fsx"
open Helper

// Day 4: Scratchcards

let part1 cards =
    cards
    |> List.map (fun (winners, num) -> Set.intersect (Set num) (Set winners))
    |> List.filter (Set.isEmpty >> not)
    |> List.sumBy (fun xs -> 2.0 ** ((float (Set.count xs)) - 1.0))
    // Part 1 result: 20667 took: 1,022µs // took 12 minutes to write solution

let part2 cards =
    // Approach :
    // * We get the amount of matches for each card
    // * We create a Map with the # of instance of each card, starting at 1
    // * we use unfold on the card & map where we:
    //   * calculate the # of cards for the next run of numbers
    //   * we merge those calculated numbers with our existing list
    //   * we return the instances of the current card, the remaining cards, and our updated map
    // * finally we sum the unfolded result
    cards
    |> List.map (fun (winners, num) -> Set.intersect (Set num) (Set winners) |> Set.count)
    |> List.mapi mkTuple
    |> fun xs -> xs, List.init (List.length xs) (fun i -> i, 1) |> Map // start with one instance each
    |> List.unfold (fun (remaining, m) ->
        match remaining with
        | [] -> None
        | (i,matches)::xs ->
            let instances = m |> Map.find i
            let update = [ for k in 1..matches -> (i+k, instances)] |> Map
            let m = Map.merge (+) m update
            Some (instances, (xs, m)))
    |> List.sum
    // Part 2 result: 5833065 took: 17,850µs // took 47 minutes to write solution (part1 + part2)

let parseLine line =
    let [winners; nums] = String.capture ":(.*) \|(.*)$" line
    String.findMatching "\d+" winners, String.findMatching "\d+" nums

let cards =
    Puzzle.readLines "day04.txt"
    |> Seq.map parseLine
    |> Seq.toList

Puzzle.warmup part1 part2 cards // warm it up for more accurate timings

Puzzle.measurePart1µs part1 cards
Puzzle.measurePart2µs part2 cards