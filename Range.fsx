#I __SOURCE_DIRECTORY__

type Range = { From: int64; To : int64 }

module Range =

    let containsValue v range =
        v >= range.From && v <= range.To

    let removeValue v range =
        if v <= range.From then [range]
        elif range.To <= v then [range]
        else [ { From = range.From; To = v - 1L }
               { From = v + 1L; To = range.To } ]

    let addScalar v range =
        { From = range.From + v; To = range.To + v }

    let size range =
        abs(range.To - range.From) + 1L

    let ensureOrder range =
        if range.From > range.To
        then { From = range.To; To = range.From }
        else range

    let tryIntersect { From = aFrom; To = aTo } { From = bFrom; To = bTo } =
        let rFrom = max aFrom bFrom
        let rTo = min aTo bTo
        if rFrom > rTo then None
        else Some { From = rFrom; To = rTo }

    /// Breaks apart the first range into parts based on how it intersects
    /// with the second range. First part is to the left of the second range,
    /// second part is any intersection, and last part is to the right of the
    /// second range
    /// e.g. breakRange (Range 1 9) (Range 5 15) = (Some (Range 1 4), Some (Range 5 9), None)
    /// e.g. breakRange (Range 1 20) (Range 5 15) = (Some (Range 1 4), Some (Range 5 15), Some (Range 16 20))
    /// e.g. breakRange (Range 5 15) (Range 1 20) = (None, Some (Range 5 15), None)
    let splitRange a b =
        match tryIntersect a b with
        | None ->
            if a.From < b.From
            then (Some a, None, None)
            else (None, None, Some a)
        | Some intersection ->
            let left =
                if a.From < intersection.From
                then Some { From = a.From; To = intersection.From - 1L }
                else None
            let right =
                if a.To > intersection.To
                then Some { From = intersection.To + 1L; To = a.To }
                else None
            (left, Some intersection, right)

    /// Removes the items that are in range b, from range a. Result will have
    /// 0, 1 or 2 results
    /// e.g. difference (Range 1 9) (Range 5 15) = [ (Range 1 4) ]
    /// e.g. difference (Range 1 20) (Range 5 15) = [ (Range 1 4) (Range 16 20) ]
    /// e.g. difference (Range 5 15) (Range 1 20) = []
    let difference a b =
        match tryIntersect a b with
        | None -> [a]
        | Some intersection ->
            let left =
                if a.From < intersection.From
                then Some { From = a.From; To = intersection.From - 1L }
                else None
            let right =
                if a.To > intersection.To
                then Some { From = intersection.To + 1L; To = a.To }
                else None
            [left; right] |> List.choose id

    /// Breaks the two ranges into 1, 2, or 3 distinct ranges with no overlap
    let breakIntoDistinctRanges a b =
        match tryIntersect a b with
        | None ->
            if a.From < b.From
            then (Some a, None, Some b)
            else (Some b, None, Some a)
        | Some intersection ->
            let leftMost = min a.From b.From
            let rightMost = max a.To b.To
            let left =
                if leftMost < intersection.From
                then Some { From = leftMost; To = intersection.From - 1L }
                else None
            let right =
                if rightMost > intersection.To
                then Some { From = intersection.To + 1L; To = rightMost }
                else None
            (left, Some intersection, right)

    /// Find items that are only in RangeA and RangeB, not in both. Result will
    /// be 2 optional ranges
    let symmetricDifference a b =
        let (left, _, right) = breakIntoDistinctRanges a b
        (left, right)

    let tryUnion { From = aFrom; To = aTo } { From = bFrom; To = bTo } =
        if (aTo >= bFrom && aFrom <= bTo) || (bTo >= aFrom && bFrom <= aTo) then
            let rFrom = min aFrom bFrom
            let rTo = max aTo bTo
            Some { From = rFrom; To = rTo }
        else
            None

    let union aRange bRange =
        match tryUnion aRange bRange with
        | Some u -> u
        | None -> failwithf "No possible union between: %A and %A" aRange bRange

    let toValues range : int64 List =
        [ for i in range.From..range.To -> i ]

    let fromValues values : Range list =
        let values =
            List.sort values
            |> List.distinct

        (values, [])
        ||> List.foldBack (fun v ranges  ->
            match ranges with
            | [] -> [ { From = v; To = v } ]
            | r::xrs ->
                if r.From - 1L = v
                then { r with From = v }::xrs
                else { From = v; To = v }::r::xrs)

    let simplify (ranges: Range List) : Range List =
        let ranges = ranges |> List.sort
        (ranges, [])
        ||> List.foldBack (fun r ranges  ->
            match ranges with
            | [] -> [ r ]
            | x::xrs ->
                match tryIntersect x r with
                | Some _ -> (union x r)::xrs
                | None -> r::x::xrs)

    /// Finds the potential range we could have when we have to add one number
    /// in a range to a number in another range
    let add { From = aFrom; To = aTo } { From = bFrom; To = bTo } =
        // e.g. [1..3] + [3..6]
        // minimum possible value is 3, largest possible is 9
        // new range is [3..9]
        { From = aFrom + bFrom; To = aTo + bTo }

    /// Finds the potential range we could have when we have to subtract one
    /// number in a range from a number in another range
    /// * Note that this is not the inverse of add! (should it have a different name?)
    let subtract { From = aFrom; To = aTo } { From = bFrom; To = bTo } =
       // e.g. [4..9] - [1..3] = [1..8]
       // minimum possible amount is 1 (4-[1]=3), largest possible is 8 (9-[8]=1)
       { From = aFrom - bTo; To = aTo - bFrom }
       // if we try to add we get a very different result.. both try to find
       // the largest potential range, doing that extends the possibilities
       // [1..8] + [1..3] = [2..11]
