#load "./Helper.fsx"
open Helper

// Day 6: Wait For It
// Approach: Ridiculously easy?!

let numberOfWins (time : int64, dist : int64) =
    List.init (int time + 1) (fun t ->
        let t = int64 t
        (time - t) * t)
    |> List.filter (fun x -> x > dist)
    |> List.length

let part1 input =
    input
    |> Seq.map (String.findMatching "\d+" >> List.map int64)
    |> fun seq -> List.zip (Seq.item 0 seq) (Seq.item 1 seq)
    |> List.map numberOfWins
    |> List.reduce (*)
    // Part 1 result: 3317888 took: 2,019µs // took 11 mins to write solution

let part2 input =
    input
    |> Seq.map (String.findMatching "\d+" >> String.concat "" >> int64)
    |> fun seq -> (Seq.item 0 seq), (Seq.item 1 seq)
    |> numberOfWins
    // Part 2 result: 24655068 took: 4,336ms // took 14 mins to write solution (part1 + part2)

let input = Puzzle.readLines "day06.txt"

Puzzle.warmup part1 part2 input // warm it up for more accurate timings

Puzzle.measurePart1µs part1 input
Puzzle.measurePart2ms part2 input