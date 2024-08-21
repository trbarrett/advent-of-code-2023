#load "./Helper.fsx"
open System.Collections.Generic
open Helper


// ??..??...?##. 1,1,3
// Could do a depth first or breadth first search.

// Q: Does it matter which end we start at? - A: No, not really

// Q: Can we remember solutions or partial? A: Maybe. Not sure yet. With a tree
// we are remembering to a large degree, but if a following section is the same
// we don't remember that.

// Q: Can we break up or simplify the solution before hand?
// A: We can break it up by `.` s. But there's not always those `.`s in
// solutions. If we simplify the solution first we might be able to force some
// more '.'s, and then we could use to split it up.

// Q: What would a simplify look like? What would the rules be?

// Start simple
// For # we have to match immediately to create a long enough run
// For . we have to end a match immediately to create the correct run length
// For ? we can work from the left to try to simplify. So for "?#?, 1" we
// start thinking ? is possible, then when we go to # we can force it to be a .

// That's ok with simple runs, but with longer runs of ???? we can't enforce the
// number of matches in a run, which makes it hard to simplify any further runs.
// It's not imposible, but the constraints increase a lot.

// So while I think we could "simplify" for some cases, when it gets to the more
// complex ones it won't add much of a benifit I don't think.

// Q: What about remembering previous solutions?
// A: A tree search does this to some respects, but only at the begininng of the
// string. As they branch off they could find themselves in similar situations,
// but have to recompute it. We can use memozation to do that. For each position
// that follows a '.' (including one's we've solved for a '?'), we can remember
// the remaining groups, and solve for that, recording the amount of matches.

// Can we do that going forwards in a dynamic programming way? I can't see why
// not. After we finish a match we record what the remainder would be from that
// position. That's doing a tree again though because there's multiple possible
// matches. It's kinda the same thing. Hmm.


// ?###???????? 3,2,1
// .###???????? 3,2,1
// .###.??????? (3),2,1

// Now we have a tree of possibilities
// .###.##.???? (3),2,1
// .###..##.??? (3),2,1
// .###...##.?? (3),2,1
// .###....##.? (3),2,1

// For dynamic programming we want to know how many solutions we have for
// ??????? 2,1
// which we can sub divide into
// ??????? 2,1
// .?????? 2,1
// ..????? 2,1
// ...???? 2,1 - 1
// ....??? 2,1 - 0
// etc

// ????? 2,1
// can further be divided into
// #???? + .???? - The last one we should have already figured out
// so we can just solve #???? which would be ???, 1, which we can do further with
// dynamic programming

// That's the memoization approach though. I think memoization is probably
// the way to go.

// real examples:
// ???????#??? 3,4
// ??????#????????????? 4,6,1,1,1

// Looking at the above I think looking for a '.' is an optimization I should
// probably ignore. When they get to part 2 where we have to unfold it we
// get things like this:
//
// ??????#????????????????????#????????????????????#????????????????????#????????????????????#????????????? 4,6,1,1,1,4,6,1,1,1,4,6,1,1,1,4,6,1,1,1,4,6,1,1,1
// which are huge runs without any breaks to optimize for. I think memoization is the only way


// if the cache is per solution, we just need to keep the counts, we'll see how
// it goes

// We need an error case I think
// e.g. .... (no cases) is valid, with 0 solutions
// e.g. .#.. (no cases) is invalid
// e.g. .... 1 is also invalid

let rec solve (springs : char list) (contiguous: int []) (cache : IDictionary<_,_>) =
    // move past any '.' out at the beginning that we can't match against
    let springs = springs |> List.skipWhile ((=) '.')

    if Array.length contiguous = 0 && List.forall ((<>) '#') springs then
        // we have a base (dumb) solution
        cache.[(springs, contiguous)] <- Some 1
        Some 1
    elif Array.length contiguous > 0 && List.isEmpty springs then
        // we have run out of springs, but still have things we need to match
        cache.[(springs, contiguous)] <- None
        None
    else

    // Check the cache
    if cache.ContainsKey(springs, contiguous)
    then cache.[(springs, contiguous)]
    else

    let minToFit = contiguous |> Array.sum
    let remainingPositions =  springs |> List.filter (fun x -> x = '?' || x = '#') |> List.length
    //printfn $"springs: {Seq.sprintnb springs}, contiguous: {Seq.sprintn contiguous}, minToFit: {minToFit}, remainingPositions: {remainingPositions}"
    if remainingPositions < minToFit then
        // if there's no way to fit the remaining runs, cache that and exit
        cache.[(springs, contiguous)] <- None
        None
    else
        // try to fit the run from the start
        let fits = springs |> List.take contiguous.[0] |> List.forall (fun x -> x = '?' || x = '#')
        // if the next char after the run would be a '#' it's not a fit because the run
        // is actually too long
        let followingIsGood =
            let followingItem = Seq.tryItem (contiguous.[0]) springs
            //printfn $"followingItem: {followingItem}"
            match followingItem with
            | Some '#' -> false
            | _ -> true

        if (not fits || not followingIsGood) && springs.[0] = '#' then
            // We can't match where we need to. This option is a bust!
            cache.[(springs, contiguous)] <- None
            None
        else
            // We might have a match, so see what the result of that would be
            // (depth first search)
            let fitMatches =
                if not fits || not followingIsGood
                then Some 0
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
                then Some 0
                else solve (List.skip 1 springs) contiguous cache

            //printfn $"springs: {Seq.sprintnb springs}, contiguous: {Seq.sprintn contiguous}, fitMatches: {fitMatches}, nextMatches: {nextMatches}"
            let result =
                match fitMatches, nextMatches with
                | None, None -> None
                | None, Some y -> Some y
                | Some x, None -> Some x
                | Some x, Some y -> Some (x + y)

            cache.[(springs, contiguous)] <- result
            result




    // * try to fit the first contiguous run in
    //   (if the next char after the run would be a '#' it's not a fit)
    //   * but just beacause the first fit doesn't work, doesn't mean we can't reject it and keep trying
    //   * Give up when remaining ?+# is less than remaining runs
    // * if it can't fit, return add the case for memoization and a 0
    // * if it does fit:
    //   - we need to see if the rest of the contiguous runs would fit
    //     * clear the next spring, and recurse
    //     * memoize the result
    //   - we need to check for different variations


let convertLine line =
    let [|springs; contiguous|] = String.split ' ' line
    springs |> List.ofSeq, String.split ',' contiguous |> Array.map int

let input = Puzzle.readLinesA "day12_2.txt" |> Array.map convertLine

let part1 lines =
    let cache = new Dictionary<char list * int array, int option>()
    let results = lines |> Array.map(fun (springs, contiguous) -> solve springs contiguous cache)
    let results = results |> Array.map Option.get
    //Seq.printns results
    results |> Array.sum


//Puzzle.warmup part1 part2 input // warm it up for more accurate timings
//Puzzle.measurePart1ms part1 input
//Puzzle.measurePart2ms part2 input

let cache = new Dictionary<char list * int array, int option>()
let (springs, contiguous) = convertLine "??.?????????#??? 3,4"
solve springs contiguous cache
|> printfn "%A"