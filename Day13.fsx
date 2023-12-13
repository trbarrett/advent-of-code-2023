#load "./Helper.fsx"
open Helper

// Day 13: Point of Incidence
// Part 1: For each pattern, pair each neighbouring row or column and check if
//         they're equal. If they are equal step outward recursively and check
//         each outer row/column to see if they're equal until we run out of
//         rows to check.
//
// Part 2: Similar to above, except our recursive function takes into account
//         smudges by counting the number of matching chars. If we have an exact
//         match keep recursing on the smudge finding function. The trick is if
//         we have an off-by-one match we use the old function that just checks
//         for exact matches when we recurse. And if we never switch to the old
//         function and run out of rows to check, we never found a smudge, and
//         therefor failed.

let rec checkForReflection (pattern : char [][]) indexLeft indexRight =
    if indexLeft < 0 || indexRight >= pattern.Length
    then true
    elif pattern[indexLeft] = pattern[indexRight]
        then checkForReflection pattern (indexLeft - 1) (indexRight + 1)
        else false

let findReflection checkReflectionFn (pattern : char [][]) =
    Array.init pattern.Length id |> Array.pairwise
    |> Array.tryFind (fun (left, right) -> checkReflectionFn pattern left right)
    |> Option.map snd

let findPatternReflectionValue checkReflectionFn (pattern : char [][]) =
    findReflection checkReflectionFn pattern |> Option.map ((*) 100)
    |> Option.orElseWith (fun () ->
        findReflection checkReflectionFn (Array.transpose pattern))
    |> Option.get

let part1 patterns =
    patterns |> Array.map (findPatternReflectionValue checkForReflection) |> Array.sum
    // Part 1 result: 29165 took: 225µs // took 17 mins to write solution

let rec checkForSmudgeReflection (pattern : char [][]) indexLeft indexRight =
    if indexLeft < 0 || indexRight >= pattern.Length
    then false // we haven't found any smudges! The instructions say we must find one!
    else
        let smudgeCount =
            Array.zip pattern[indexLeft] pattern[indexRight]
            |> Array.filter ((<||) (<>)) |> Array.length
        match smudgeCount with
        | 0 -> checkForSmudgeReflection pattern (indexLeft - 1) (indexRight + 1)
        | 1 -> checkForReflection pattern (indexLeft - 1) (indexRight + 1)
        | _ -> false

let part2 patterns =
    patterns |> Array.map (findPatternReflectionValue checkForSmudgeReflection) |> Array.sum
    // Part 2 result: 32192 took: 824µs // took 65 mins to write solution (part1 + part2)

let patterns =
    Puzzle.readLinesA "day13.txt" |> Array.split ""
    |> Array.map (Array.map Seq.toArray)

Puzzle.warmup part1 part2 patterns // warm it up for more accurate timings
Puzzle.measurePart1µs part1 patterns
Puzzle.measurePart2µs part2 patterns