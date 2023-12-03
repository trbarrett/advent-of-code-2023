#I __SOURCE_DIRECTORY__

open System
open System.Text.RegularExpressions
open System.IO
open System.Collections.Generic

let flip f a b = f b a
let mkTuple x y = x, y

let tee f x = f x; x

let startStopwatch () = System.Diagnostics.Stopwatch.StartNew ()

module Puzzle =

    let warmup f1 f2 input =
        f1 input |> ignore
        f2 input |> ignore

    let measurePartms partN f input =
        let sw = startStopwatch ()
        let result = f input
        printfn $"Part {partN} result: {result} took: {sw.ElapsedMilliseconds}ms"

    let measurePart1ms f input = measurePartms 1 f input
    let measurePart2ms f input = measurePartms 2 f input

    let measurePartµs partN f input =
        let sw = startStopwatch ()
        let result = f input
        let µs = sw.ElapsedTicks / 1000L
        printfn $"Part {partN} result: {result} took: {µs:N0}µs"

    let measurePart1µs f input = measurePartµs 1 f input
    let measurePart2µs f input = measurePartµs 2 f input

    let public readLines inputName =
        sprintf "%s/puzzledata/%s" __SOURCE_DIRECTORY__ inputName
        |> File.ReadLines

    let public readLinesWithHashComments inputName =
        sprintf "%s/puzzledata/%s" __SOURCE_DIRECTORY__ inputName
        |> File.ReadLines
        |> Seq.filter (fun (str: String) -> not (str.StartsWith('#')))

    let public readLinesWithSlashComments inputName =
        sprintf "%s/puzzledata/%s" __SOURCE_DIRECTORY__ inputName
        |> File.ReadLines
        |> Seq.filter (fun (str: String) -> not (str.StartsWith("//")))


let (|KeyValue|) (keyValuePair : KeyValuePair<'k, 'v>) : 'k * 'v =
    let k = keyValuePair.Key
    let v = keyValuePair.Value
    (k, v)

module Tuple =
    let flip (x,y) = y, x

module Map =
    let addToListValue key value m =
        match m |> Map.tryFind key with
        | None -> m |> Map.add key [value]
        | Some existing -> m |> Map.add key (value::existing)

    let ofTuples xs : Map<'a, 'b list> =
        Seq.fold (fun m (x,y) -> m |> addToListValue x y) Map.empty xs

    let flattenOneToMany m =
        let flatten = (fun (KeyValue(x, ys)) -> ys |> Seq.map (mkTuple x))
        Seq.collect flatten m

    let keys map =
        map |> Map.keys |> seq

    let mapValues f m =
       m |> Map.map (fun _ v -> f v)

    let merge f mapA mapB =
       let allKeys = [mapA; mapB] |> Seq.collect keys |> Seq.distinct

       (Map [], allKeys)
       ||> Seq.fold (fun acc key ->
           let aValue = mapA |> Map.tryFind key
           let bValue = mapB |> Map.tryFind key
           match aValue, bValue with
           | None, None -> failwith "Something has gone very wrong"
           | None, Some bValue -> acc |> Map.add key bValue
           | Some aValue, None -> acc |> Map.add key aValue
           | Some aValue, Some bValue -> acc |> Map.add key (f aValue bValue))

    let mergeMany f maps =
       let allKeys = maps |> Seq.collect keys |> Seq.distinct

       (Map [], allKeys)
       ||> Seq.fold (fun acc key ->
           // find the values in all the maps for a given key and combine them
           // with function f
           let values = maps |> Seq.map (Map.tryFind key)
           let values = values |> Seq.choose id
           acc |> Map.add key (f values))

    // Update a value in a map by first finding it, calling a given function
    // with that value (or a default if none was found), then setting it back
    // in the Map
    let update key defaultValue f m =
        let existing = m |> Map.tryFind key |> Option.defaultValue defaultValue
        m |> Map.add key (f existing)

module Set =

    let getExtents points =
        let x,y = Set.minElement points
        ((x, x, y, y), points)
        ||> Seq.fold (fun (xMin, xMax, yMin, yMax) (x, y) ->
            min xMin x, max xMax x, min yMin y, max yMax y)

    // If the set contains points (x,y tuples), then this function will print
    // out the full grid, with '.' for empty spaces, '#' for points within
    // the set, and 'o' for the origin
    let printSetPoints tails =
        let (xMin, xMax, yMin, yMax) = getExtents tails
        [yMax..(-1)..yMin]
        |> List.iter (fun y ->
            [xMin..xMax]
            |> List.iter (fun x ->
                if (x,y) = (0,0) then printf "o"
                elif Set.contains (x,y) tails then printf "#"
                else printf ".")
            printfn "")

    let printSetPointsBottomToTop tails =
        let (xMin, xMax, yMin, yMax) = getExtents tails
        [yMin..yMax]
        |> List.iter (fun y ->
            [xMin..xMax]
            |> List.iter (fun x ->
                if (x,y) = (0,0) then printf "o"
                elif Set.contains (x,y) tails then printf "#"
                else printf ".")
            printfn "")

module List =
    let permutationsWithReplacement (values : 'a list) times =
        let splitValues = values |> List.map List.singleton
        let folder acc values =
            List.allPairs acc values
            |> List.map (fun (xs, x) -> x::xs)
        List.fold folder splitValues (List.replicate (times - 1) values)

    // When the order DOES matter, it is a permutation
    let rec permutations (values : Set<'a>) count : 'a list list =
        if count = 0 then [[]]
        else
            values |> List.ofSeq
            |> List.collect (fun x ->
               permutations (Set.remove x values) (count - 1)
               |> List.map (fun xs -> x::xs))

    // When the order doesn't matter, it is a combination
    let rec combinations n l =
        match n, l with
        | 0, _ -> [[]]
        | _, [] -> []
        | k, (x::xs) ->
            List.map ((@) [x]) (combinations (k-1) xs) @ combinations k xs

    let replaceAt replaceAt replacement xs =
        xs
        |> List.mapi (fun i x ->
            if i = replaceAt then replacement
            else x)

    let tryExtractFirst pred lst =
        let rec loop lst frnt =
            match lst with
            | x::xs when pred x ->
                Some x, (List.rev frnt) @ xs
            | x::xs -> loop xs (x::frnt)
            | [] -> None, List.rev frnt
        loop lst []

    let extractFirst pred lst =
        let rec loop lst frnt =
            match lst with
            | x::xs when pred x ->
                x, (List.rev frnt) @ xs
            | x::xs -> loop xs (x::frnt)
            | [] -> failwithf "Found nothing to match the predicate in given list"
        loop lst []

    let removeLast lst =
        match lst with
        | [] -> []
        | lst -> List.removeAt (List.length lst - 1) lst

    /// Given a list of tuples, and treating each tuple as key/value pairs,
    /// this function will combine the values where there are duplicate pairs
    /// with the same key
    let combineKeyValuePairs reducer lst =
        lst
        |> List.groupBy fst
        |> List.map (fun (key, grp) ->
            key, (grp |> List.map snd |> List.reduce reducer ))

    let groupByTuple (xs : ('a * 'b) list) =
        xs
        |> List.groupBy fst
        |> List.map (fun (k,v) -> k, v |> List.map snd)

    let partitionBy (fn : 'x -> Choice<'a,'b>)  (lst: 'x list) =
        (lst, ([],[]))
        ||> List.foldBack (fun x (accA, accB) ->
            match fn x with
            | Choice1Of2 a -> (a::accA, accB)
            | Choice2Of2 b -> (accA, b::accB))

    let partition3WaysBy (fn : 'x -> Choice<'a,'b,'c>)  (lst: 'x list) =
        (lst, ([],[],[]))
        ||> List.foldBack (fun x (accA, accB, accC) ->
            match fn x with
            | Choice1Of3 a -> (a::accA, accB, accC)
            | Choice2Of3 b -> (accA, b::accB, accC)
            | Choice3Of3 c -> (accA, accB, c::accC))

module Array =

    // Works the same as takeWhile, except it will include the first item that
    // matches the function if there is one
    let takeWithFirst f xs =
        match Array.tryFindIndex (f >> not) xs with
        | Some idx -> Array.sub xs 0 (idx + 1)
        | None -> Array.copy xs

module Seq =
    let groupByTuple (xs : ('a * 'b) seq) =
        xs
        |> Seq.groupBy fst
        |> Seq.map (fun (k,v) -> k, v |> Seq.map snd)

    let split separator seq =
        (seq, [[]])
        ||> Seq.foldBack (fun x acc ->
            match x, acc with
            | x, _ when x = separator -> []::acc
            | _, curr::rest -> (x::curr)::rest
            | _ -> failwith "Not Possible")

    let toString (separator : string) (s : seq<'a>) =
        String.Join(separator, s)

    let tryMin (s : seq<'a>) =
        (None, s)
        ||> Seq.fold (fun acc x ->
            match acc with
            | None -> Some x
            | Some y -> Some (min x y))

    let tryMax (s : seq<'a>) =
        (None, s)
        ||> Seq.fold (fun acc x ->
            match acc with
            | None -> Some x
            | Some y -> Some (max x y))


module String =

    let fromChars (chars : seq<char>) : string =
     new String(Array.ofSeq chars)

    let split (delimiter : char) (input : string) =
        input.Split delimiter

    let splitStr (delimiter : string) (input : string) =
        input.Split delimiter

    let reverse (input : string) =
        input.ToCharArray() |> Seq.rev |> fromChars

    let trim (input : string) = input.Trim()

    let findMatching regexPattern (input : string) =
        Regex.Matches(input, regexPattern)
        |> Seq.map (fun x -> x.Value)
        |> List.ofSeq

    let capture regexPattern (input : string) =
        Regex.Match(input, regexPattern).Groups
        |> Seq.skip 1
        |> Seq.map (fun x -> x.Value)
        |> List.ofSeq

    let captureAll regexPattern (input : string) =
        Regex.Matches(input, regexPattern)
        |> Seq.map (fun m ->
            m.Groups
            |> Seq.skip 1
            |> Seq.map (fun x -> x.Value)
            |> List.ofSeq)
        |> List.ofSeq

    let captureAllWithIndex regexPattern (input : string) =
        Regex.Matches(input, regexPattern)
        |> Seq.map (fun m ->
            m.Groups
            |> Seq.skip 1
            |> Seq.map (fun x -> x.Index, x.Value)
            |> List.ofSeq)
        |> List.ofSeq

module ArrayOfArrays =
    let tryFindIndex predicate (aoa : 'T[][]) =
        let rec loop rowNo =
            if rowNo >= aoa.Length then None
            else
                match Array.tryFindIndex predicate aoa[rowNo] with
                | Some colNo -> Some (rowNo, colNo)
                | None -> loop (rowNo + 1)

        loop 0

    let findIndexes predicate (aoa : 'T[][]) =
        [| for rowNo in 0..(aoa.Length - 1) do
               for colNo in 0..(aoa[rowNo].Length - 1) do
                   if (predicate aoa.[rowNo].[colNo])
                      then yield rowNo, colNo |]

    let get (rowNo, colNo) (aoa : 'T[][]) =
        aoa.[rowNo].[colNo]

    let tryGet (rowNo, colNo) (aoa : 'T[][]) =
        if rowNo < 0 || rowNo >= aoa.Length then None
        elif colNo < 0 || colNo >= aoa[rowNo].Length then None
        else Some aoa.[rowNo].[colNo]

    let tryGeti (rowNo, colNo) (aoa : 'T[][]) =
        if rowNo < 0 || rowNo >= aoa.Length then None
        elif colNo < 0 || colNo >= aoa[rowNo].Length then None
        else Some ((rowNo, colNo), aoa.[rowNo].[colNo])

    let map (mapping: 'T -> 'U) (aoa : 'T[][]) =
        [| for rowNo in 0..(aoa.Length - 1) do
               [| for colNo in 0..(aoa[rowNo].Length - 1) do
                      yield mapping aoa.[rowNo].[colNo] |] |]

    let mapi (mapping: int -> int -> 'T -> 'U) (aoa : 'T[][]) =
        [| for rowNo in 0..(aoa.Length - 1) do
               [| for colNo in 0..(aoa[rowNo].Length - 1) do
                      yield (mapping rowNo colNo aoa.[rowNo].[colNo]) |] |]

    let transpose = Array.transpose

let (|StartsWith|_|) (p:string) (s:string) =
    if s.StartsWith(p)
    then Some(s.Substring(p.Length))
    else None

let (|Capture|_|) regex (s:string) =
    match String.capture regex s with
    | [] -> None
    | items -> Some(items)

module Char =
    let digitToInt (c : char) = int c - int '0'

module Int32 =
    let tryParse (str : string) =
        match Int32.TryParse str with
        | true, int -> Ok int
        | _ -> Error (sprintf "Could not parse int from string: '%s'" str)

    let tryParseOpt (str : string) =
        match Int32.TryParse str with
        | true, int -> Some int
        | _ -> None

module Math =

    /// Takes a seq of int64 digits and converts them into a single number
    /// based on position. e.g. [ 4L; 5L; 7L; 2L; 7L ] -> 45727L
    let digitsToInt64 digits =
        digits
        |> Seq.rev
        |> Seq.mapi (fun i x -> (int64 (pown 10 i)) * x )
        |> Seq.sum

    /// Takes a seq of int32 digits and converts them into a single number
    /// based on position. e.g. [ 4; 5; 7; 2; 7 ] -> 45727
    let digitsToInt32 digits =
        digits
        |> Seq.rev
        |> Seq.mapi (fun i x -> (pown 10 i) * x )
        |> Seq.sum

    /// Takes a seq of 1 or 0 digits and converts them into a single number
    /// based on position. e.g. [ 1; 0; 1; 1; 0 ] -> 22
    let binaryDigitsToInt32 digits =
        digits
        |> Seq.rev
        |> Seq.mapi (fun i x -> (int32 (pown 2 i)) * x )
        |> Seq.sum


let splitOnEmptyLines seq = Seq.split "" seq

let memoize f =
    let dict = Dictionary<_,_>()
    fun x ->
        if dict.ContainsKey x then
            dict.[x]
        else
            let value = f x
            dict.Add(x, value)
            value

let memoize2 f =
    let dict = Dictionary<_,_>()
    fun a b ->
        if dict.ContainsKey (a,b) then
            dict.[(a,b)]
        else
            let value = f a b
            dict.Add((a,b), value)
            value

