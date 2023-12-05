#load "./Helper.fsx"
#load "./Range.fsx"
open Helper
open Range

// Day 5: If You Give A Seed A Fertilizer
// Approach : Use ranges and intersections of ranges
//   Part 2 :
//   If we think of seed as a long range, and the mappings as sub ranges then
//   we just need just step through each intersection and non-intersection
//   and apply the mappings (or none).
//   That will give us a set of new ranges that are all over the shop. We do
//   that for each seed, and combine all the ranges into one long list and do
//   it again with the next group of mappings
//
//   seed 1   : x--------------------------------------------------x
//   mappings : ----mmmmmm-----------mmmmmmmmmmmmm----------mmmmm---
//   after    : x--x      x---------x             x--------x     x-x
//            :        x---------x           x--x     x-------x    x------x
//

type Mapping = { Source : Range; Dest : int64 }

let mapGroup seeds mappingGroup =
    seeds
    |> List.map (fun seed ->
        mappingGroup
        |> List.tryFind (fun mapping -> Range.containsValue seed mapping.Source)
        |> Option.map (fun mapping -> mapping.Dest + (seed - mapping.Source.From))
        |> Option.defaultValue seed)

let part1 (seeds, mappingGroups) =
    (seeds, mappingGroups) ||> List.fold mapGroup |> List.min
    // Part 1 result:  322500873 took: 59µs // took 23 minutes to write solution

// The mappings are sorted by source, otherwise this wouldn't work
let rec mapSeedRange seed mappings acc =
    match seed, mappings with
    | None, _ -> acc
    | Some seed, [] -> seed::acc
    | Some seed, mapping::xs ->
        let left, intersect, right = Range.splitRange seed mapping.Source

        let acc =
            match left with
            | Some range -> range::acc // no mapping for these numbers
            | _ -> acc

        let acc =
            match intersect with
            | Some range -> // apply the mapping for these numbers
                Range.addScalar (mapping.Dest - mapping.Source.From) range::acc
            | _ -> acc

        mapSeedRange right xs acc // map the rest of the seeds

let mapGroupRanges seeds mappingGroup =
    seeds |> List.collect (fun seed -> mapSeedRange (Some seed) mappingGroup [])

let part2 (seeds, mappingGroups) =
    let seeds =
        seeds |> List.chunkBySize 2
        |> List.map (fun [x;l] -> { From = x; To = x + l })
    (seeds, mappingGroups) ||> List.fold mapGroupRanges
    |> List.min |> fun x -> x.From
    // Part 2 result: 108956227 took: 144µs // took 1h 53m to write solution (part1 + part2)

let parseGroup lines =
    lines
    |> List.skip 1
    |> List.map (String.findMatching "\d+" >> List.map int64)
    |> List.map (fun [dest; source; range] ->
        { Source = { From = source; To = source + range - 1L }
          Dest = dest })
    |> List.sortBy (fun x -> x.Source.From)

let input =
    let first::rest = Puzzle.readLines "day05.txt" |> Seq.toList
    let seeds = String.findMatching "\d+" first |> List.map int64
    let groups = rest |> Seq.skip 1 |> Seq.split "" |> List.map parseGroup
    seeds, groups

Puzzle.warmup part1 part2 input // warm it up for more accurate timings

Puzzle.measurePart1µs part1 input
Puzzle.measurePart2µs part2 input