#I __SOURCE_DIRECTORY__

open System.Collections.Generic

let rec private dijkstra' (neighbours : 'T -> ('T * int) list)
                          (target : 'T option)
                          (visited : SortedDictionary<'T, int>)
                          (frontier : SortedSet<int * 'T>) =
    if frontier.Count = 0
    then visited
    else
        let cost, curr = frontier.Min
        frontier.Remove (cost, curr) |> ignore

        if Some curr = target
        then visited // we've reached our goal
        else if visited.ContainsKey curr
        then dijkstra' neighbours target visited frontier // we've already been here, continue
        else

        visited.Add(curr, cost) |> ignore

        // get the neighbours and their travel costs
        let adjacent =
            neighbours curr
            |> List.map (fun (item, value) -> (item, value + cost))

        // add their costs in the frontier
        // note: there's a good change we could be adding an item twice, that's
        // ok though. When we run into it the second time it will already be
        // in the visited list and ignored
        for item, value in adjacent do
            frontier.Add((value, item)) |> ignore

        dijkstra' neighbours target visited frontier

let dijkstra (start : 'T) (target : 'T) (neighbours : 'T -> ('T * int) list) =

    let initialFrontier = new SortedSet<int * 'T>()
    initialFrontier.Add(0,start) |> ignore

    let costs =
        dijkstra'
            neighbours
            (Some target)
            (new SortedDictionary<'T, int>())
            (initialFrontier)

    match costs.TryGetValue target with
    | true, cost -> Some cost
    | _ -> None

let dijkstraAllPaths (start : 'T) (neighbours : 'T -> ('T * int) list) =

    let initialFrontier = new SortedSet<int * 'T>()
    initialFrontier.Add(0,start) |> ignore

    let costs =
        dijkstra'
            neighbours
            None
            (new SortedDictionary<'T, int>())
            (initialFrontier)

    costs |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map

