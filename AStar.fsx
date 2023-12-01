#I __SOURCE_DIRECTORY__

open System
open System.Collections.Generic
open System.Linq

// A* finds a path from start to goal.
// `neighbours` find all the neighbouring states from a given state
// `h` is the heuristic function. h(n) estimates the cost to reach goal from node n.
let astar start goal neighbours h =
    let inline dictAddOrUpdate key value (dict : IDictionary<_,_>) =
        if dict.ContainsKey key
        then dict.[key] <- value
        else dict.Add(key, value)

    let rec reconstructPath (cameFrom : Dictionary<_,_>) current totalPath =
        match cameFrom.TryGetValue(current) with
        | false, _ -> totalPath
        | true, current -> reconstructPath cameFrom current (current::totalPath)

    // Use a Map since it's a sorted tree. Seq |> head will get you the minElement
    let rec processNeighbour (openSet : HashSet<_>)
                             (cameFrom : Dictionary<_,_>)
                             (gScore : Dictionary<_,double>)
                             (fScore : SortedDictionary<_,SortedSet<_>>)
                             current
                             (neighbor, neighborScore) =
        // d(current,neighbor) is the weight of the edge from current to neighbor
        // tentative_gScore is the distance from start to the neighbor through current
        let tentativeGScore =
            gScore.GetValueOrDefault(current, Double.PositiveInfinity)
            + neighborScore

        let neighborScore =
            gScore.GetValueOrDefault(neighbor, Double.PositiveInfinity)

        if tentativeGScore < neighborScore
        then
            // This path to neighbor is better than any previous one. Record it!
            cameFrom |> dictAddOrUpdate neighbor current
            gScore |> dictAddOrUpdate neighbor tentativeGScore
            let fullPathScore = (tentativeGScore + (h neighbor goal))
            let items = fScore.GetValueOrDefault(fullPathScore, SortedSet ())
            items.Add(neighbor) |> ignore
            fScore |> dictAddOrUpdate fullPathScore items
            openSet.Add(neighbor) |> ignore

    let rec mainLoop (openSet : HashSet<_>)
                     (cameFrom : Dictionary<_,_>)
                     (gScore : Dictionary<_,double>)
                     (fScore : SortedDictionary<_,SortedSet<_>>) =
        let fScoreHead = fScore.First()
        let current = fScoreHead.Value.Min
        if fScoreHead.Value.Count > 1
        then fScoreHead.Value.Remove(current) |> ignore
        else fScore.Remove(fScoreHead.Key) |> ignore

        if current = goal
        then Some (reconstructPath cameFrom current [goal], gScore.[goal])
        elif openSet.Count = 0
        then None
        else
            openSet.Remove(current) |> ignore
            let neighbours = neighbours current

            for neighbour in neighbours do
                processNeighbour openSet cameFrom gScore fScore current neighbour

            mainLoop openSet cameFrom gScore fScore

    // The set of discovered nodes that may need to be (re-)expanded.
    // Initially, only the start node is known.
    // This is usually implemented as a min-heap or priority queue rather than a hash-set.
    let openSet = HashSet([start]) // we won't add current to the set, because we changed the recursion order

    // For node n, cameFrom[n] is the node immediately preceding it on the cheapest path from start
    // to n currently known.
    let cameFrom = Dictionary<_,_>()

    // For node n, gScore[n] is the cost of the cheapest path from start to n currently known.
    //gScore := map with default value of Infinity
    let gScore = Dictionary<_,_>() //{ } Map [start, 0.]
    gScore.Add(start, 0.)

    // For node n, fScore[n] := gScore[n] + h(n). fScore[n] represents our current best guess as to
    // how cheap a path could be from start to finish if it goes through n.
    let fScore = SortedDictionary<_,_>()
    fScore.Add((h start goal), SortedSet([start]))

    mainLoop openSet cameFrom gScore fScore

