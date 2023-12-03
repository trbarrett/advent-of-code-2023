#load "./Helper.fsx"
open Helper

// Day 3: Gear Ratios
// Approach:
//  * Parse numbers and symbols separately, and give each a position
//  * Search for symbols surrounding each number
//  * In part 2, group numbers with * symbols

let findSurroundingSymbols (x, y, number) symbols =
    let potentialPositions =
        Set [ for y in (y-1)..(y+1) do
                for x in (x-1)..(x+(String.length number)) -> x,y ]
    symbols
    |> List.filter (fun (x, y, _) ->
        Set.contains (x,y) potentialPositions)

let part1 (numbers : (int * int * string) list, symbols : (int * int * string) list) =
    numbers
    |> List.filter (fun number ->
        findSurroundingSymbols number symbols
        |> List.isEmpty |> not)
    |> List.sumBy (fun (_, _, number) -> int number)
    // Part 1 result: 535078 took: 183ms // took 27 minutes to write solution

let part2 (numbers : (int * int * string) list, symbols : (int * int * string) list) =
    let symbols = symbols |> List.filter (fun (_,_,symbol) -> symbol = "*")
    numbers
    |> List.collect (fun number ->
        findSurroundingSymbols number symbols
        |> List.map (fun symbol -> symbol, number) )
    |> List.groupByTuple
    |> List.filter (fun (_, values) -> List.length values = 2)
    |> Seq.sumBy (fun (_,[(_,_,num1);(_,_,num2)]) -> (int num1) * (int num2))
    // Part 2 result: 75312571 took: 97ms // took 35 minutes to write solution (part1 + part2)

let parseLine lineNo line =
    let addLineNo (items : (int * string) list) =
        items |> List.map (fun (x, num) -> (x, lineNo, num))

    let numbers = String.captureAllWithIndex "(\d+)" line |> List.map List.head
    let symbols = String.captureAllWithIndex "([^.\d])" line |> List.map List.head
    (numbers |> addLineNo, symbols |> addLineNo)

let schematic =
    Puzzle.readLines "day03.txt"
    |> Seq.mapi parseLine
    |> Seq.toList
    |> List.unzip
    |> fun (numbers, symbols) -> List.concat numbers, List.concat symbols

Puzzle.warmup part1 part2 schematic // warm it up for more accurate timings

Puzzle.measurePart1ms part1 schematic
Puzzle.measurePart2ms part2 schematic
