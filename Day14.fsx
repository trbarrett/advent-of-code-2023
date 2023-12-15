#load "./Helper.fsx"
open Helper

// Day 14: Parabolic Reflector Dish
//
// Part 2 Approach: Attempt to look for cycles of numbers in the load after each
//                  dish rotation. Use the cycle count to figure out which index
//                  will occur on the 1 billionth cycle
//
// Note: Did everything in a purely functional way. It made this quite slow, but
// not enough to really worry about. Would certainly be cleaner with some mutation

let rockFall rocks =
    let folder i (acc, stopIndex) x =
        match x with
        | '#' ->
            let emptySpaces = i- stopIndex - 1
            let empty = List.replicate emptySpaces '.'
            (x::empty@acc, i)
        | '.' -> (acc, stopIndex)
        | 'O' -> (x::acc, stopIndex + 1)

    let result = rocks |> Array.foldi folder (List.empty, -1) |> fst

    // need to add empty space that might be missing
    let result =
        if result.Length < rocks.Length then
            let empty = List.replicate (rocks.Length - result.Length) '.'
            empty@result
        else result

    result |> List.rev |> List.toArray

type Dir = North | East | South | West

let tiltDish rocks =
    rocks |> Array.transpose |> Array.map rockFall |> Array.transpose

let calculateLoad rocks =
    rocks |> Array.mapi (fun i row ->
        row
        |> Array.filter ((=) 'O')
        |> Array.length
        |> ((*) (Array.length rocks - i)))
    |> Array.sum

let part1 rocks =
    tiltDish rocks |> calculateLoad
    // Part 1 result: 105249 took: 741µs // took 35 mins to write solution

let rotateDish rocks = // Hacky hack hack
    // North
    rocks |> Array.transpose |> Array.map rockFall |> Array.transpose
    // West
    |> Array.map rockFall
    // South
    |> ArrayOfArrays.flipVertically |> Array.transpose |> Array.map rockFall
    |> Array.transpose |> ArrayOfArrays.flipVertically
    // East
    |> ArrayOfArrays.flipHorizontally |> Array.map rockFall
    |> ArrayOfArrays.flipHorizontally

let everyNth n lst =
    [ for i in 0..((List.length lst / n) - 1) -> List.item (i * n) lst ]

let findLoopCycleLength loads =
    let loads = Array.rev loads
    let loopIndexes = Array.findIndexes ((=) loads.[0]) loads
    let indexGroupings = [ for x in 1..5 -> everyNth x loopIndexes]
    indexGroupings
    |> List.pick (fun loopIndexes ->
        if List.length loopIndexes < 10 then None
        else
            let allMatching =
                loopIndexes |> List.pairwise |> List.take 10
                |> List.map (fun (i, j) ->  Array.sub loads i (j - i))
                |> List.pairwise |> List.forall ((<||) (=))
            if allMatching
            then Some (loopIndexes.[1] - loopIndexes.[0])
            else None)

let cycles = 1_000_000_000

let part2 rocks =
    let rotateDish (rocks, loads) =
        let rocks = rotateDish rocks
        let load = calculateLoad rocks
        rocks, load::loads
    let doRotations = List.replicate 600 rotateDish |> List.reduce (>>)
    let loads = doRotations (rocks, []) |> snd |> List.rev |> List.toArray
    let loopCycleLength = loads |> findLoopCycleLength
    let remainingCycles = (cycles - (Array.length loads)) / loopCycleLength
    let rem = cycles - (remainingCycles * loopCycleLength + (Array.length loads))
    (Array.rev loads).[loopCycleLength - rem]
    // Part 2 result: 88680 took: 1,695ms // Forgot to time it :( 2.5h roughly?

let rocks = Puzzle.readLinesA "day14.txt" |> Array.map Seq.toArray

Puzzle.warmup part1 part2 rocks // warm it up for more accurate timings
Puzzle.measurePart1µs part1 rocks
Puzzle.measurePart2ms part2 rocks