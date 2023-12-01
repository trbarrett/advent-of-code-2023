#load "./Helper.fsx"
open Helper

// Day 1: Trebuchet?!
//
// Part 1 - Concat the first and last numbers of each line, then sum them
// Part 2 - Concat the first and last numbers of each line then sum them. Each
//          number may be written as a word e.g "one". The hard part here was to
//          use a regex lookahead to capture overlapping words, i.e. "oneight"

let part1 lines =
    lines
    |> Seq.map (String.splitIntoMatching "\d")
    |> Seq.filter (List.isEmpty >> not)
    |> Seq.map (fun digits ->  int64 $"{digits.[0]}{List.last digits}")
    |> Seq.sum
    // Correct Answer: 56397, took: 2,611µs

let wordToDigit = function
    | "one" ->   "1" | "two" ->   "2" | "three" -> "3"
    | "four" ->  "4" | "five" ->  "5" | "six" ->   "6"
    | "seven" -> "7" | "eight" -> "8" | "nine" ->  "9"
    | str -> str

let part2 lines =
    lines
    |> Seq.map (String.captures "(?=(one|two|three|four|five|six|seven|eight|nine|\d))")
    |> Seq.map (List.map List.head) // we care about the first (and only) group in each capture
    |> Seq.map (fun digits -> int64 $"{wordToDigit digits.[0]}{wordToDigit (List.last digits)}")
    |> Seq.sum
    // Correct Answer: 55701, took: 8,599µs

let lines = Puzzle.readLines "day01.txt"

Puzzle.warmup part1 part2 lines // warm it up for more accurate timings

Puzzle.measurePart1µs part1 lines
Puzzle.measurePart2µs part2 lines
