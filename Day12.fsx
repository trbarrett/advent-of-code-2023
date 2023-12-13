#load "./Helper.fsx"
open Helper

// Day 12:

// We replace the first ? we find, then grab up to that point as a group.
// then we take the amount of groups we have and see if they're still valid
// If it's not valid we backtrack.
// We want to try all valid options, so we try replacing with a . and replacing
// with a # and see what happens

let isCompleteValid (springs : string) (contiguous : int []) =
    let brokenGroups = springs |> String.split '.' |> Array.filter ((<>) "")
    if brokenGroups.Length <> contiguous.Length
    then false
    else
        Array.zip brokenGroups contiguous
        |> Array.forall (fun (broken, contiguous) -> broken.Length = contiguous)

let isIncompleteValid index (springs : string) (contiguous : int []) =
    let brokenGroups =
        springs.Substring(0, index)
        |> String.split '.' |> Array.filter ((<>) "")

    // my concern is replacing with '.' will be valid for a long time, even
    // though it's not actually possible. Is there a quick way to test for that?
    // is it even worth it performance wise?


    // we could check the potential groups including ?. They need to be more than the contigious
    // |> String.split '.' |> Array.filter ((<>) "")

    if Array.length brokenGroups = 0
    then true
    else
        let comparisons = Seq.zip brokenGroups contiguous
        let last = Seq.last comparisons
        let comparisons = Seq.take (Seq.length comparisons - 1) comparisons
        let completedMatch = comparisons |> Seq.forall (fun (broken, contiguous) -> broken.Length = contiguous)
        if completedMatch
        then
            let (broken, contiguous) = last
            broken.Length <= contiguous
        else false

    // last group may be unfishished, so it doesn't need to complete, just less than the max

//let arr = [|1; 1; 3|]
//let str = "#.?.###"
//let i = 3
//let isValid = isIncompleteValid 1 "#??.###" arr
//printfn $"isValid: {isValid} for i:{i}, '{str}' and %A{arr}"


let rec fitSprings index (springs : string) contiguous =
    //printfn $"fitSprings i:{index} - {springs}"
    // TODO check when complete
    if index = springs.Length
    then
        if isCompleteValid springs contiguous
        then [springs]
        else []
    elif not (isIncompleteValid index springs contiguous)
    then
        //printfn $"invalid i:{index} - {springs}"
        []
    else
        if springs.[index] <> '?' then
            fitSprings (index + 1) springs contiguous
        else
            let springsA = String.replaceAt index '.' springs
            let springsB = String.replaceAt index '#' springs
            fitSprings (index + 1) springsA contiguous
            @ fitSprings (index + 1) springsB contiguous


    // We can't place a # if it would create a contiguous line more
    // than X long
    //| ("?#"::xs)::ys, 1 ->  fail
    //| ("?"::xs)::ys, 1 ->  works
    //| ("#?"::xs)::ys, 1 ->  works


let processSprings (springs, contiguous) =
    fitSprings 0 springs contiguous
    //let springs =
    //    springs |> String.split '.' |> Array.filter ((<>) "")
    //    |> Seq.toList

    // try the first place we can put a broken spring

    //|> Seq.printn

// +20 m
let part1 input =
    input |> Array.map processSprings |> Array.map List.length
    |> Array.sum
    // Part 1 result: 7718 took: 627ms // took 61 mins to write solution

// -5 m

// part 2 - 3h 25m so far

let expandInput (springs, contiguous : int []) =
    Seq.join "?" (Array.replicate 5 springs),
    Array.replicate 5 contiguous |> Array.concat


let part2 input =
    let input = input |> Array.map expandInput
    input |> Array.take 1 |> Array.map processSprings |> Array.map List.length
    |> Array.sum

let getOverlapCount (str : string) =
    [ for i in str.Length..(-1)..1 do
        if i = str.Length then (i, 1)
        elif str.[str.Length - i - 1] = '?' then (i, 1) ]
    |> Map

    // #### = 4,1
    // #### i: 4 = (4,1)
    // #### i: 3 = 4 - 3 - 1= 1 - not ?, skip
    // #### i: 2 = 4 - 2 - 1= 2 - not ?, skip
    // #### i: 1 = 4 - 1 - 1= 3 - not ?, skip

    // ?### = 4,1; 3,1
    // ?### i: 4 = (4,1)
    // ?### i: 3 = 4 - 3 - 1= 1 - ? so 3,1
    // ?### i: 2 = 4 - 2 - 1= 2 - not ?, skip
    // ?### i: 1 = 4 - 1 - 1= 3 - not ?, skip

    // #??# = 4,1; 2,1, 1,1
    // #??# i: 4 = (4,1)
    // #??# i: 3 = 4 - 3 - 1= 1 - not ? so skip
    // #??# i: 2 = 4 - 2 - 1= 2 - is '?', so (2,1)
    // #??# i: 1 = 4 - 1 - 1= 3 - is '?', so (1,1)

    // ????

    //[ for i in 0..(str.Length - 1) do
    //    if (str.Length - i - 1) < 0
    //    then (i, 1)
    //    elif str.[str.Length - i - 1] = '?'
    //    then (i, 1) ]
    // #### = 4,1

    // #### = i = 0,  str.Length - i - 1 =


let getContiguousPart prevCalcs (str : string) =
    // get overlapping cases -- this could be masivley simplified
    //let overlaps = List.init str.Length (fun x -> (x+1, 1)) |> Map
    let overlaps = getOverlapCount str

    //'???'
    // 012
    let lastWild = str.LastIndexOf '?'
    let strCalc =
        if lastWild <= 0 then overlaps
        else
            let prev = prevCalcs |> Map.find (str.Substring(0, lastWild))
            Map.merge (+) overlaps prev
    prevCalcs |> Map.add str strCalc


let getRun (str : string) =
    [ for i in str.Length..(-1)..1 do
        if i = str.Length then (i, 1)
        elif str.[str.Length - i - 1] = '?' then (i, 1) ]
    |> Map

    // #### = 4,1
    // #### i: 4 = (4,1)
    // #### i: 3 = 4 - 3 - 1= 1 - not ?, skip
    // #### i: 2 = 4 - 2 - 1= 2 - not ?, skip
    // #### i: 1 = 4 - 1 - 1= 3 - not ?, skip

    // ?### = 4,1; 3,1
    // ?### i: 4 = (4,1)
    // ?### i: 3 = 4 - 3 - 1= 1 - ? so 3,1
    // ?### i: 2 = 4 - 2 - 1= 2 - not ?, skip
    // ?### i: 1 = 4 - 1 - 1= 3 - not ?, skip

    // #??# = 4,1; 2,1, 1,1
    // #??# i: 4 = (4,1)
    // #??# i: 3 = 4 - 3 - 1= 1 - not ? so skip
    // #??# i: 2 = 4 - 2 - 1= 2 - is '?', so (2,1)
    // #??# i: 1 = 4 - 1 - 1= 3 - is '?', so (1,1)


let getContiguous2 prevCalcs (str : string) =
    let wildIndexes = str |> Seq.toArray |> Array.findIndexes ((=) '?')

    // The hard part is where we have a mix of # and ?.
    // If we have #?# there are only 2 options. 1 and 1 or 3

    [ for i in 0..(str.Length-1) do

        if wildIndexes |> List.concat i
        then
            let known = prevCalcs |> Map.find (str.Substring(0, lastWild))
        let overlap = getRun (str.Substring(i+1)) ]

    [ for i in wildIndexes do
        let known = prevCalcs |> Map.find (str.Substring(0, lastWild))
        let overlap = getRun (str.Substring(i+1)) ]

// #?#?
// i = 1  -> 0 + 1, 2
// i = 3  -> 1 or 2 + 2 + 1


//
//
//    //    Map [ for x in str.Length..(-1)..1 do if x > 0 then yield (x, 1) ]
//    // get the contiguous calculations for the substring
//    let prev = prevCalcs |> Map.find (str.Substring(0, str.Length - 1))
//    //let prev =
//    //    if str.Length = 1
//    //    then prevCalcs |> Map.find (str.Substring(0, str.Length - 1))
//    //    else Map.empty
//    let strCalc = Map.merge (+) overlaps prev
//    prevCalcs |> Map.add str strCalc

let contiguous (str : string) =
    let m : Map<string,Map<int,int>> = Map ["", Map.empty]
    let subStrings = List.init str.Length (fun x -> str.Substring(0,x+1))
    (m, subStrings)
    ||> List.fold getContiguousPart
    |> Map.find str

contiguous "#??#??" |> Seq.printn

// contiguous "#??#???" = [1, 4],[2, 4],[3, 2],[4, 3],[5, 2],[6, 1],[7, 1]

// It's something, but does it help match the contiguous ordering?
// Kind of, if it's 1 - 1 - 1 then we can use it.
// But if our contiguous data is 3 - 1 - 2... then no
// How can we change it to handle that???
// Do we store combinations of runs instead?
// So fo "#??#??" instead of "[1, 3],[2, 3],[3, 2],[4, 2],[5, 1],[6, 1]"
// we would have :[1;2]:1, [2;1]:1, [1;2]:2, [1:3]:2, [2:2]:2, etc
// It would take up more space - but be more useful for the puzzle
// But do we really need that? - We just need to keep the matches for contiguous
// runs that are listed. I think so. Although we could start excluding runs we
// don't need, then we'd lose track of where we are I think.
// In the example it has a case of 506250 arrangements. If we keep all the
// run combinations that could get a bit crazy. Might be ok though. Strings
// can be a max of 20 in the puzzle input. So 20*5 +5, 105. Hopefully it's not
// a crazy amount of combinations. And we can cull one's that aren't valid



//let part2 input =
    // Can we solve it with Dynamic Programming?
    // Solve for 0-1 and 1st contiguous - How many valid?
    // Solve for 0-2 and 1st contiguous - How many valid?
    // Can we use the previous result? - Doesn't seem so? If we have "??" and
    // we put a "#" in the first "?" we have something valid for just that range.
    // But a following "#" would make it invalid.
    //  The validity depends on the previous item - I feel like that makes it
    //  hard to do classic DP... and yet DP feels like a good approach
    // Maybe this is the wrong way to think about it. What if we thought in
    // terms of recursive functions? Do we event need to take contiguous as a
    // parameter? Can we figure out all the contiguous options then figure out
    // what matches afterwards???
    // ?  =  1:1
    // ?? =  prev(1:1) + 1:1 + overlap (2:1) = 1:2, 2:1
    // ??? = prev(1:2, 2:1) + 1:1 + overlaps (2:1, 3:1) = 1.1, 1:3, 2:2, 3:1
    // might work, but I'm not sure how the overlap is working to be honest?
    // How do we know what we might overlap with? I guess we just calculate it
    // by looking at every opportunity for this length?
    // For length 3 there is no other option. We just have 3
    // For length 2 there is no new option. We just have 2
    // For length 1 there is 1 + the variations for the first ?
    // It feels like we have to leave a gap of one, then do the variations. That
    // makes sense

//let part2 input =
    // ideas????
    // we can't just add them one by one. We need to find a way to multiply.
    // Note: We don't need to record each matching string. Just the number,
    // So for a subset we can forget about each matching string we've found,
    // just the number. So for 5 chunks, we might have 5 matches of just 1
    // contiguous group, 3 matches for 2 contiguous groups, and 2 matches for 3
    // contiguous groups.
    //
    // Q: What would that data structure look like?
    // A: We have an index we've reached in the group, and hash with number
    // of contiguous groups matched so far, and how many matches total
    //
    // Q: How can we do that in an inductive way?
    // A: So we have that existing data structure, and an index. Hmm. What next
    //    So if it's a '.' We just do nothing and keep moving. Fine
    //    If it's a '#', then it's the start or continuation of a group.
    //  BUT that doesn't seem quite right. Continuations of groups doesn't really
    //  mesh with our previous data structure. We either need to keep a partial
    //  group as a parameter that we could add to and finish, or somehow go into
    //  a subroutine to deal with a potential chunk of data. I don't like the
    //  subroutine option much as we have cases which are just 1 giant chunk. So
    //  we need to remember unfinished runs. I think our data structure with the count
    //  of contiguous groups could keep unfinished run with it.
    //
    // Q: How do we know we only need to keep 1 unfinished run with each count
    // of contiguous groups?
    //
    //
    //
    //
    // Part 2 result: 363293506944 took: 241ms // took 47 mins to write solution (part1 + part2)

let input =
    Puzzle.readLinesA "day12.txt"
    |> Array.map (fun line ->
        let [|springs; contiguous|] = String.split ' ' line
        springs, String.split ',' contiguous |> Array.map int)

//Puzzle.warmup part1 part2 input // warm it up for more accurate timings
//Puzzle.measurePart1ms part1 input
//Puzzle.measurePart2ms part2 input