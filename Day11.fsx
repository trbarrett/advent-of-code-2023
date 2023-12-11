#load "./Helper.fsx"
open Helper

// Day 11: Cosmic Expansion
// Part 1: Approach: duplicate rows/columns with no stars, then use manhattan distance
// Part 2: Approach: remember expanded rows/columns, then do manhattan distance
//         step by step, adding 1 million if it's an expanded row/column
//
// My pre-existing List.combinations function made this easier

let expand sky =
    (sky, []) ||> Array.foldBack (fun row acc ->
        if Array.forall ((=) '.') row
        then row::row::acc
        else row::acc)
    |> List.toArray

let manhattanDistance ((row1, col1), (row2, col2)) =
    abs(row1 - row2) + abs(col1 - col2)

let starCombinations stars =
    stars |> Array.toList |> List.combinations 2 |> List.map (fun [a;b] -> a,b)

let part1 sky =
    let sky = sky |> expand |> Array.transpose |> expand |> Array.transpose
    let stars = ArrayOfArrays.findIndexes ((=) '#') sky
    stars |> starCombinations |> List.map manhattanDistance |> List.sum
    // Part 1 result: 9233514 took: 43ms // took 21 mins to write solution

let expandedRowsAndColumnIndexes sky =
    let rows = sky |> Array.findIndexes (Array.forall ((=) '.'))
    let cols = sky |> Array.transpose |> Array.findIndexes (Array.forall ((=) '.'))
    Set rows, Set cols

let starDistance expRows expCols ((row1, col1), (row2, col2)) =
    let expandAmt = 1_000_000L
    let rowStep = if row2 - row1 > 0 then 1 else -1
    let colStep = if col2 - col1 > 0 then 1 else -1
    let rowDistance =
        [ for r in row1..rowStep..(row2 - rowStep) do
            if Set.contains r expRows then expandAmt else 1L ]
    let colDistance =
        [ for c in col1..colStep..(col2 - colStep) do
            if Set.contains c expCols then expandAmt else 1L ]
    List.sum rowDistance + List.sum colDistance

let part2 sky =
    let expRows, expCols = expandedRowsAndColumnIndexes sky
    ArrayOfArrays.findIndexes ((=) '#') sky |> starCombinations
    |> List.map (starDistance expRows expCols) |> List.sum
    // Part 2 result: 363293506944 took: 241ms // took 47 mins to write solution (part1 + part2)

let sky = Puzzle.readLinesA "day11.txt" |> Array.map (Array.ofSeq)

Puzzle.warmup part1 part2 sky // warm it up for more accurate timings
Puzzle.measurePart1ms part1 sky
Puzzle.measurePart2ms part2 sky