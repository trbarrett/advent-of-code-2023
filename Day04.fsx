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
    // * We create a List with the # of instance of each card (each starting with 1)
    // * we use unfold on the card & instances list where we:
    //   * increase the # of instances based on the # of matches we have
    //   * return the instances of the current card, the remaining cards, and our updated map
    // * finally we sum the unfolded result
    cards
    |> List.map (fun (winners, num) -> Set.intersect (Set num) (Set winners) |> Set.count)
    |> fun xs -> xs, List.replicate (List.length xs) 1
    |> List.unfold (fun (remaining, il) ->
        match remaining, il with
        | [], _ -> None
        | matches::xs, instances::il ->
            let il = il |> List.mapi (fun i x -> if i < matches then x + instances else x)
            Some (instances, (xs, il)))
    |> List.sum
    // Part 2 result: 5833065 took: 1,978µs // took 47 minutes to write solution (part1 + part2)

let parseLine line =
    let [winners; nums] = String.capture ":(.*) \|(.*)$" line
    String.findMatching "\d+" winners, String.findMatching "\d+" nums

let cards =
    Puzzle.readLines "day04.txt" |> Seq.toList
    |> List.map parseLine

Puzzle.warmup part1 part2 cards // warm it up for more accurate timings

Puzzle.measurePart1µs part1 cards
Puzzle.measurePart2µs part2 cards