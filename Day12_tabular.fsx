#load "./Helper.fsx"
open System.Collections.Generic
open Helper

// Day 12: Hot Springs
// Approach:  Tabular Dynamic Programming
//   We step through each of the problems one char at a time, updating a list
//   of each possible states that could occur for that problem given the groups,
//   and how many times that same state could occur. Then at the end we just
//   count how many states are at the final state including variations

// The first time I tried a tabular approach I cast my net too wide. Trying to
// create a generic table for any number of possible groups. It took me a few
// hints to get that I should make it more specific for the individual problem,
// but since this is the first time I've actually written tabular DP code I
// don't mind it being a bit of a learning exercise. We're only keeping the
// results from the last char, so I'm not sure it's a pure "tabular" approach

let describe (springs : char list) (contiguous: int []) =
    (String.fromChars springs) + " " + (Seq.toString "," contiguous)
    

type State =
    | MatchComplete // We've finished matching all the groups
    | LookingForNextMatch of groupIndex: int // waiting to start matching the next group
    | AttemptingMatch of charIndex: int * groupIndex: int // in the middle of matching a group

let considerSingleState (contiguous: int []) springIdx ch tableItem =
    // two cases, we're working to match a group or we need to start a new group
    match tableItem with
    | AttemptingMatch (charIndex, groupIndex) ->
        let groupLength = contiguous.[groupIndex]
        // if the list has ended, we need to see if next char is valid for a break.
        if springIdx = charIndex + groupLength then // charIndex + groupLength will be the char after the end
            // see if the list is still valid if it is, continue
            if ch = '#' then
                //If it's a '#' then we shouldn't have ended, and we've got an invalid state
                []
            else
                // we've finished the group
                if groupIndex = Array.length contiguous - 1 then
                    // we've finished matching the final group
                    [ MatchComplete ]
                else
                    [ LookingForNextMatch (groupIndex + 1) ]
        elif ch = '.' then
            // we've run into a empty space in the middle of a group. It's invalid
            []
        else
            // we're still good (either a '#' or '?'), so keep going
            [ tableItem ]
    | MatchComplete ->
        // I've we've already completed all our matches, we can't match a
        // new '#', so the state is invalid. abandon it
        if ch = '#' then []
        else [ MatchComplete ] 
    | LookingForNextMatch groupIndex ->
        if ch = '.' then
            [ tableItem ] 
        elif ch = '#' then
            [ AttemptingMatch (springIdx, groupIndex) ]
        elif ch = '?' then
            // we should keep the same item state for treating '?' as a '.' ,
            // but also start a new one treating '?' as a '#'.
            [ tableItem
              AttemptingMatch (springIdx, groupIndex) ]
        else
            failwith $"Unrecognised char: {ch}"

let solve (springs : char list) (contiguous: int []) =
    // add an extra . to the end of the springs. It means we won't need an extra
    // step at the end to check each match
    let springs = springs @ ['.']
    
    // We start our table with needing to match the first group
    let startTable = [ LookingForNextMatch 0, 1L ]
    
    (startTable, springs)
    ||> List.foldi (fun i acc ch ->
        // update each state in the table
        acc
        |> List.map (fun (tableItem, count) ->
            let results = considerSingleState contiguous i ch tableItem
            results |> List.map (fun x -> x, count))
        // each state could result in one, zero, or multiple states for this char
        // so we need to flatten them all out
        |> List.concat
        |> List.combineKeyValuePairs (+) // combine when the states are the same
    )
    |> List.filter (fun (state, _) -> state = MatchComplete)
    |> List.sumBy snd

let convertLine line =
    let [|springs; contiguous|] = String.split ' ' line
    springs |> List.ofSeq, String.split ',' contiguous |> Array.map int

let input = Puzzle.readLinesA "day12.txt" |> Array.map convertLine

let part1 lines =
    lines
    |> Array.map(fun (springs, contiguous) -> solve springs contiguous)
    |> Array.sum
    // Part 1 result: 7718 took: 15ms
    
let expandInput (springs : char list, contiguous : int []) =
    List.join ['?'] (List.replicate 5 springs),
    Array.replicate 5 contiguous |> Array.concat
    
let part2 lines =
    let lines = lines |> Array.map expandInput
    lines
    |> Array.map(fun (springs, contiguous) -> solve springs contiguous)
    |> Array.sum
    // Part 2 result: 128741994134728 took: 152ms

Puzzle.warmup part1 part2 input // warm it up for more accurate timings
Puzzle.measurePart1ms part1 input
Puzzle.measurePart2ms part2 input