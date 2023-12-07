#load "./Helper.fsx"
open Helper

// Day 8: Haunted Wasteland
// Part 2 Approach:
//    * From each starting node follow it through the turns until it will start
//      looping through the same nodes again (could go through the list of turns
//      multiple times). Keep that history.
//    * Looking at that history, find all the different move counts that reach a
//      'Z' ending before we start looping again.
//    * Cross join those move counts for all the starting node histories and
//      find the LCM of the cross joins. The lowest one is our answer
//
// Looking at the results each path only visits a 'Z' ending once per travel, so
// I could have just got the number of moves to the first one and multiplied them
// all. Would have make this a lot simpler. Oh well.
//
// This actually has potential issues. We could run into a 'Z' before we start
// looping (think of a lasso) and we'll be using that Z for the LCM when it
// doesn't make sense.
//
// Also we don't take into account amount of time until each following cycle hits
// the Z nodes. That would affect the LCM turning this into some monster chinese
// remainder theorem problem. The only way it works is that the data was
// specifically crafted for it too work.

let step curr moveNo (moves : char []) network =
    let turn = moves[moveNo % moves.Length]
    let node = Map.find curr network
    if turn = 'L' then fst node else snd node

let part1 (moves, network) =
    let rec move curr moveNo (moves : char []) network =
        let next = step curr moveNo moves network
        if next = "ZZZ" then moveNo else move next (moveNo + 1) moves network
    let moveNo = move "AAA" 0 moves network
    moveNo + 1
    // Part 1 result: 14429 took: 1,526µs // took 20 mins to write solution

let findLoop startingNode (moves : char []) (network : Map<string, (string * string)>) =
    let rec loop curr moveNo history =
        let turnNo = moveNo % moves.Length
        let next = step curr moveNo moves network
        // if we're at the same step in the turns, and the same node we've found a loop
        match history |> Map.tryFind curr with
        | Some histItem when histItem |> List.exists (fun x -> fst x = turnNo) ->
            history
        | _ ->
            history
            |> Map.update curr [] (fun xs -> (turnNo, moveNo)::xs)
            |> loop next (moveNo + 1)

    loop startingNode 0 Map.empty

let part2 (moves, network) =
    let startingNodes =
        network |> Map.keys
        |> Seq.filter (fun (s : string) -> s.EndsWith "A") |> Seq.toList
    let loops =
        startingNodes |> List.map (fun start -> findLoop start moves network)

    let zSteps =
        loops
        |> List.collect (fun loop ->
            loop
            |> Map.filter (fun k _ -> k.EndsWith "Z")
            |> Map.values
            |> Seq.map (List.map (snd >> int64))
            |> Seq.toList)

    let lcm =
        zSteps
        |> List.reduce (fun a b ->
            List.crossJoin2 a b
            |> List.map ((<||) Math.lcm)
            |> List.distinct)

    lcm |> List.sort |> List.head
    // Part 2 result: 10921547990923 took: 144ms
    // took 1h 10mins (with a lunch break in the middle) to write solution (part1 + part2)

let input =
    let [moves; nodes] = Puzzle.readLines "day08.txt" |> Seq.split ""
    moves.[0].ToCharArray(),
    nodes |> List.map (String.capture "(\w+) = \((\w+), (\w+)\)")
    |> Seq.map (fun [n;l;r] -> (n, (l,r)))
    |> Map

Puzzle.warmup part1 part2 input // warm it up for more accurate timings

Puzzle.measurePart1µs part1 input
Puzzle.measurePart2ms part2 input