#load "./Helper.fsx"
open Helper

// Day 15: Lens Library
// Super easy, just some basic hash table operations and maths

let hashAlgo chars =
    (0, chars) ||> Array.fold (fun acc c -> (acc + (int c)) * 17 % 256)

let part1 initSequence =
    initSequence |> Array.map hashAlgo |> Array.sum
    // Part 1 result: 509167 took: 122µs // took 8 mins to write solution

let performOpToBoxes (boxes : Map<int, (char [] * int) list>) op =
    if Array.last op = '-' then
        let label = (Array.sub op 0 ((Array.length op) - 1))
        let boxNo = hashAlgo label
        boxes |> Map.change boxNo (function
            | None -> None
            | Some xs -> Some (List.filter (fun lens -> fst lens <> label) xs))
    else
        let [|label; focalLength|] = Array.split '=' op
        let boxNo = hashAlgo label
        let focalLength = int (String.fromChars focalLength)
        Map.updateListValue boxNo (label, focalLength) (fun v -> fst v = label) boxes

let calcFocusingPower (boxes : Map<int, (_ * int) list>) =
    boxes |> Map.toList
    |> List.sumBy (fun (box, xs) ->
        xs
        |> List.mapi (fun i (_, focalLength) ->
            (box + 1) * (i + 1) * focalLength)
        |> List.sum)

let part2 initSequence =
    (Map.empty, initSequence) ||> Array.fold performOpToBoxes
    |> Map.map (fun _ v -> List.rev v) // because list is built backwards
    |> calcFocusingPower
    // Part 1 result: 259333 took: 4,176µs // took 39 mins to write solution (part1 + part2)

let initSequence =
    Puzzle.readLinesA "day15.txt" |> Array.head
    |> String.split ',' |> Array.map Seq.toArray

Puzzle.warmup part1 part2 initSequence // warm it up for more accurate timings
Puzzle.measurePart1µs part1 initSequence
Puzzle.measurePart1µs part2 initSequence