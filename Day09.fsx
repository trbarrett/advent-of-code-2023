#load "./Helper.fsx"
open Helper

// Day 9: Mirage Maintenance
// Approach - Recursion: Today was a cakewalk

let rec calculateNextInSeries numbers =
    if Array.forall ((=) 0L) numbers then 0L
    else
        let differences = numbers |> Array.pairwise |> Array.map (fun (a, b) -> b - a)
        (calculateNextInSeries differences) + numbers.[numbers.Length - 1]

let rec calculatePrevInSeries numbers =
    if Array.forall ((=) 0L) numbers then 0L
    else
        let differences = numbers |> Array.pairwise |> Array.map (fun (a, b) -> b - a)
        numbers.[0] - (calculatePrevInSeries differences)

let part1 input =
    input |> List.map calculateNextInSeries |> List.sum
    // Part 1 result: 1806615041 took: 765µs // took 20 mins to write solution... didn't handle negatives :'(

let part2 input =
    input |> List.map calculatePrevInSeries |> List.sum
    // Part 2 result: 1211 took: 978µs // took 24 mins to write solution (part1 + part2)

let input =
    Puzzle.readLinesL "day09.txt"
    |> List.map (String.splitStr " " >> Array.map int64)

Puzzle.warmup part1 part2 input // warm it up for more accurate timings

Puzzle.measurePart1µs part1 input
Puzzle.measurePart2µs part2 input