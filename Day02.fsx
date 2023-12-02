#load "./Helper.fsx"
open Helper

// Day 2: Cube Conundrum
// Part 1 & 2: All about working with multiple MultiSets (Bags)
// Approach:
//   For each line:
//   * Put the cubes in a map for each subgame
//   * Merge the maps, always picking the largest when there's a collision
//   * Do whats needed to solve the rest of the puzzle part

let part1 games =
    let maxCubes = Map ["red", 12; "green", 13; "blue", 14]
    games
    |> Seq.filter (fun (_, subgames) ->
        subgames
        |> Seq.append [maxCubes] // including the max sets a minimum threshold
        |> Map.mergeMany Seq.max
        |> fun subgames -> subgames <= maxCubes)
    |> Seq.sumBy fst
    // Part 1 result: 2256 took: 847µs

let part2 games =
    games
    |> Seq.map (fun (_, subgames) ->
        subgames
        |> Map.mergeMany Seq.max
        |> Map.values
        |> Seq.reduce (*))
    |> Seq.sum
    // Part 2 result: 74229 took: 460µs

let parseLine line =
    let [gameId; subgames] = String.capture "^Game (\d+): (.*)$" line
    let subgames =
        subgames
        |> String.split ';'
        |> Array.map (String.captureAllMatching "(\d+) (\w+),?")
        |> Array.map (List.map (fun [count; colour] -> colour, int count) >> Map)
    int gameId, subgames

let games =
    Puzzle.readLines "day02.txt"
    |> Seq.map parseLine
    |> Seq.toList

Puzzle.warmup part1 part2 games // warm it up for more accurate timings

Puzzle.measurePart1µs part1 games
Puzzle.measurePart2µs part2 games
