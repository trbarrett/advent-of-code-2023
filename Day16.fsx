#load "./Helper.fsx"
open Helper

// Day 16: The Floor Will Be Lava
// Approach: Just run the maze. Hardest part was fixing a bug when the start
// location had a non-empty tile

type Tile = Empty | SplitterEW | SplitterNS | ReflectPos | ReflectNeg

type Dir = N | S | E | W

let next (rowNo, colNo) = function
    | N -> rowNo - 1, colNo | S -> rowNo + 1, colNo
    | W -> rowNo, colNo - 1 | E -> rowNo, colNo + 1

let navigate (cavern : char [] []) pos dir =
    let nextPos = next pos dir
    ArrayOfArrays.tryGet nextPos cavern
    |> Option.map (fun tile ->
        match dir, tile with
        | _, '.' -> [dir]
        | E, '-' | W, '-' -> [dir]
        | N, '-' | S, '-' -> [E; W]
        | N, '|' | S, '|' -> [dir]
        | E, '|' | W, '|' -> [N; S]
        | E, '/' -> [N] | W, '/' -> [S]
        | N, '/' -> [E] | S, '/' -> [W]
        | E, '\\' -> [S] | W, '\\' -> [N]
        | N, '\\' -> [W] | S, '\\' -> [E]
        | _ -> failwith $"Invalid tile: {tile}")
    |> Option.map (List.map (fun dir -> nextPos, dir))

let gameStep cavern cache curr =
    // Add to the cache. Remove any that we've previously explored
    let cache, curr =
        ((cache, []), curr)
        ||> List.fold (fun (cache, acc) item ->
            if Set.contains item cache
            then cache, acc
            else Set.add item cache, item::acc)

    // Move all the beams forward
    let nextState =
        curr
        |> List.choose (fun (pos, dir) ->
            navigate cavern pos dir)
        |> List.concat

    cache, nextState

//let printCavern (cavern: char array array) (cache : Set<(int * int) * Dir>) =
//    let dirMap = cache |> Map.ofSeq
//
//    cavern
//    |> Array.iteri (fun rowNo row ->
//        row |> Array.iteri (fun colNo ch ->
//            let ch =
//                match ch, dirMap |> Map.tryFind (rowNo, colNo) with
//                | '|', _ | '-', _ | '\\', _ | '/', _ -> ch
//                | _, Some N -> '^'
//                | _, Some E -> '>'
//                | _, Some W -> '<'
//                | _, Some S -> 'v'
//                | _, None -> ch
//
//            printf $"%c{ch}")
//        printfn ""
//    )

let findEnergized cavern start =
    let rec run cache state =
        let nextCache, nextState = gameStep cavern cache state
        if List.isEmpty nextState
        then nextCache
        else run nextCache nextState

    run Set.empty [start]
    |> Set.map fst
    |> Set.count
    |> fun x -> x - 1 //-1 because we start outside of the cavern

let part1 cavern =
    let start = (0,-1), E
    findEnergized cavern start
    // Part 1 result: 7632 took: 61ms // Times got lost, started before christmas then had a massive break

let part2 (cavern : char [] []) =
    let rows = Array.length cavern - 1
    let cols = Array.length cavern.[0] - 1
    let startingPoints =
        [ for i in [0..cols] -> (-1, i), S ] @
        [ for i in [0..cols] -> (rows+1, i), N ] @
        [ for i in [0..rows] -> (i, -1), E ] @
        [ for i in [0..rows] -> (i, cols+1), W ]

    startingPoints
    |> List.map (fun start -> findEnergized cavern start)
    |> List.max
    // Part 2 result: 8023 took: 12,457ms // took 10 minutes more than part 1

let cavern =
    Puzzle.readLinesA "day16.txt"
    |> Array.map Seq.toArray

Puzzle.measurePart1ms part1 cavern
Puzzle.measurePart2ms part2 cavern