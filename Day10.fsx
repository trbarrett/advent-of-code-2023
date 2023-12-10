#load "./Helper.fsx"
open Helper

// Day 10: Pipe Maze
// Part 1: Finding and following a path in a grid
// Part 2: Finding the inside of the previous path

let surroundingPoints (row, col) (tiles : char [] []) =
    // clockwise from top
    [| if row > 0 then yield (row - 1, col)
       if (col + 1) < tiles.[row].Length then yield (row, col + 1)
       if (row + 1) < tiles.Length then yield (row + 1, col)
       if col > 0 then yield (row, col - 1) |]

let validMoves (row, col) (tile : char) =
   match tile with
   | '|' -> [| (row + 1, col); (row - 1, col) |]
   | '-' -> [| (row, col + 1); (row, col - 1) |]
   | 'L' -> [| (row - 1, col); (row, col + 1) |]
   | 'J' -> [| (row - 1, col); (row, col - 1) |]
   | '7' -> [| (row + 1, col); (row, col - 1) |]
   | 'F' -> [| (row + 1, col); (row, col + 1) |]
   | _ -> [||]

let getTile (tiles : char [] [])  (row,col) = tiles.[row].[col]

let firstValidMove (row,col) (tiles : char [] []) =
   surroundingPoints (row, col) tiles
   |> Array.find (fun point ->
       validMoves point (getTile tiles point) |> Array.contains (row,col))

let nextValidMove prevPoint point (tiles : char [] []) =
    let tile = getTile tiles point
    if tile = 'S' then None // We've returned to the start
    else validMoves point tile |> Array.find ((<>) prevPoint) |> Some

let getLoopPoints start (tiles : char [] []) =
    let next = firstValidMove start tiles
    let points =
        (next, [start])
        |> Array.unfold (fun (curr, visited) ->
            nextValidMove (visited |> List.head) curr tiles
            |> Option.map (fun next -> curr, (next, curr::visited) ))
    Array.insertAt 0 start points

let part1 (start, (tiles : char [] [])) =
    getLoopPoints start tiles |> fun pts -> pts.Length / 2
    // Part 1 result: 6897 took: 5,313µs // took 1h 7mins to write solution... bugs, bugs, bugs

let floodFill p (tiles : char [] []) (barrier : (int * int) []) =
    let rec fillNext filled p =
        if Set.contains p filled || Array.contains p barrier
        then filled
        else (Set.add p filled, surroundingPoints p tiles) ||> Array.fold fillNext
    fillNext Set.empty p

type Turn = | RH | LH
type Dir = | Up | Right | Down | Left

let changeDir turn dir =
    match turn with
    | RH -> match dir with | Up -> Right | Right -> Down | Down -> Left | Left -> Up
    | LH -> match dir with | Up -> Left | Left -> Down | Down -> Right | Right -> Up

let moveInDir (row, col) = function
   | Up -> (row - 1, col) | Down -> (row + 1, col)
   | Right -> (row, col + 1) | Left -> (row, col - 1)

let dirMoved (prevRow, prevCol) (currRow, currCol) =
    match currRow - prevRow, currCol - prevCol with
    | -1, 0 -> Up | 0, 1 -> Right | 1, 0 -> Down | 0, -1 -> Left

let findTurnType prev curr tile =
    match dirMoved prev curr, tile with
    | Left,  'L' -> Some RH | Down, 'L' -> Some LH
    | Right, 'J' -> Some LH | Down, 'J' -> Some RH
    | Left,  'F' -> Some LH | Up,   'F' -> Some RH
    | Right, '7' -> Some RH | Up,   '7' -> Some LH
    | _ -> None

let findInside (tiles : char [] []) loopPath insideDir =
    let findInside' prevPoint loopPoint =
        let dirMoved = dirMoved prevPoint loopPoint
        let inside = dirMoved |> changeDir insideDir |> moveInDir loopPoint

        // Turning an inside corner has an extra point straight ahead. grrrr
        // e.g  |
        //     *L---
        //      *
        match findTurnType prevPoint loopPoint (getTile tiles loopPoint) with
        | Some LH when insideDir = RH -> [| inside; (moveInDir loopPoint dirMoved) |]
        | Some RH when insideDir = LH -> [| inside; (moveInDir loopPoint dirMoved) |]
        | _ -> [| inside |]

    loopPath
    |> Array.pairwise
    |> Array.collect (fun (prev, curr) -> findInside' prev curr)
    |> Array.distinct
    |> Array.filter (fun pt -> not (Array.contains pt loopPath))

let part2 (start, (tiles : char [] []))  =
    let loopPath = getLoopPoints start tiles
    // Get a point we know is on the outside
    let minPoint = loopPath |> Array.min
    // Reorder the path to make our minPoint the start
    let idx = System.Array.IndexOf(loopPath, minPoint)
    let array1, array2 = Array.splitAt idx loopPath
    let loopPath = Array.append array2 array1
    // next point must be to the right or down. If it's right then the inside
    // is on the RH side. If it's down than inside is on the LH side.
    let insideDir = if fst loopPath.[1] = fst minPoint then RH else LH

    // walk the path and find all the points directly inside the path, then
    // we'll do a flood-fill on each of those points
    let directlyInsidePoints = findInside tiles loopPath insideDir
    (Set.empty, directlyInsidePoints)
    ||> Array.fold (fun filled point ->
        if Set.contains point filled then filled
        else floodFill point tiles loopPath |> Set.union filled )
    |> Seq.length

    // Part 2 result: 367 took: 112ms // took roughly 3.5h to write solution (part1 + part2)
    //
    // I first went with a ray-cast method, but the collinear lines really messed
    // things up. So I switched to a following the wall method. Then got stuck
    // for a while dealing with turns where I needed to include 2 inside points
    // for the corner. May have been quicker to tough it out on the ray-cast
    // method, but oh well.

let input =
    let tiles =
        Puzzle.readLinesA "day10-example7.txt"
        |> Array.map (Array.ofSeq)

    let start = ArrayOfArrays.tryFindIndex ((=) 'S') tiles
    start |> Option.get, tiles

Puzzle.warmup part1 part2 input // warm it up for more accurate timings

Puzzle.measurePart1µs part1 input
Puzzle.measurePart2ms part2 input