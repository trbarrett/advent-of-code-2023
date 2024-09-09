#load "./Helper.fsx"
open System.Collections.Generic
open Helper

// Day 12: Hot Springs
// Approach: Depth first search with memoization (Dynamic Programming)

let describe (springs : char list) (contiguous: int []) =
    (String.fromChars springs) + " " + (Seq.toString "," contiguous)

let rec solve (springs : char list) (contiguous: int []) (cache : IDictionary<_,int64 option>) : int64 option =
    // move past any '.' out at the beginning that we can't match against
    let springs = springs |> List.skipWhile ((=) '.')

    if Array.length contiguous = 0 then
        if List.forall ((<>) '#') springs then
            // There are no springs left to match, and nothing left to match
            // against. We have a valid solution
            cache.[describe springs contiguous] <- Some 1l
            Some 1l
        else 
            // There are springs we still need to match, but nothing to match
            // them against. This isn't a valid solution
            cache.[describe springs contiguous] <- None
            None
    elif Array.length contiguous > 0 && List.isEmpty springs then
        // we have run out of springs, but still have things we need to match
        cache.[describe springs contiguous] <- None
        None
    else
        
    // Check the cache
    if cache.ContainsKey(describe springs contiguous)
    then cache.[describe springs contiguous]
    else

    let minToFit = contiguous |> Array.sum
    let remainingPositions =  springs |> List.filter (fun x -> x = '?' || x = '#') |> List.length
    if remainingPositions < minToFit then
        // if there's no way to fit the remaining runs, cache that and exit
        cache.[describe springs contiguous] <- None
        None
    else
        // try to fit the run from the start
        let fits = springs |> List.take contiguous.[0] |> List.forall (fun x -> x = '?' || x = '#')
        // if the next char after the run would be a '#' it's not a fit because the run
        // is actually too long
        let followingIsGood =
            let followingItem = Seq.tryItem contiguous.[0] springs
            match followingItem with
            | Some '#' -> false
            | _ -> true

        if (not fits || not followingIsGood) && springs.[0] = '#' then
            // We can't match where we need to. This option is a bust!
            cache.[describe springs contiguous] <- None
            None
        else
            // We might have a match, so see what the result of that would be
            // (depth first search)
            let fitMatches =
                if not fits || not followingIsGood
                then Some 0L
                else
                    let nextSprings =
                        // +1 to skip the following item
                        if List.length springs > contiguous.[0] + 1
                        then springs |> List.skip (contiguous.[0] + 1)
                        else [] // we've reached the end!
                    let nextContiguous =
                        if Array.length contiguous > 0
                        then Array.tail contiguous
                        else [||]
                    solve nextSprings nextContiguous cache

            // regardless of whether we had a match, we might be able to skip this one
            // if it's a '?' and try from the next item
            let nextMatches =
                if springs.[0] <> '?'
                then Some 0L
                else solve (List.tail springs) contiguous cache

            let result =
                match fitMatches, nextMatches with
                | None, None -> None
                | None, Some y -> Some y
                | Some x, None -> Some x
                | Some x, Some y -> Some (x + y)

            cache.[(describe springs contiguous)] <- result
            result

let convertLine line =
    let [|springs; contiguous|] = String.split ' ' line
    springs |> List.ofSeq, String.split ',' contiguous |> Array.map int

let input = Puzzle.readLinesA "day12.txt" |> Array.map convertLine

let part1 lines =
    let cache = new Dictionary<string, int64 option>()
    let results = lines |> Array.map(fun (springs, contiguous) ->
        solve springs contiguous cache)
    results |> Array.map Option.get |> Array.sum
    // Part 2 result: 7718 took: 4ms
    
let expandInput (springs : char list, contiguous : int []) =
    List.join ['?'] (List.replicate 5 springs),
    Array.replicate 5 contiguous |> Array.concat
    
let part2 lines =
    let lines = lines |> Array.map expandInput
    let cache = new Dictionary<string, int64 option>()
    let results = lines |> Array.map(fun (springs, contiguous) ->
        solve springs contiguous cache)
    results |> Array.map Option.get |> Array.sum
    // Part 2 result: 128741994134728 took: 262ms

Puzzle.warmup part1 part2 input // warm it up for more accurate timings
Puzzle.measurePart1ms part1 input
Puzzle.measurePart2ms part2 input
