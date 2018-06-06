module GeneticSteinerTree.Core.Graph.SteinerTree
open GeneticSteinerTree.Extensions
open GeneticSteinerTree.Core.Data

let combinationsWithoutRepetition2ofN elements =
   let pairHeadWithTail = function
      | [] -> Seq.empty
      | x::xs -> xs |> Seq.map (fun y -> x, y)
   List.collectTail pairHeadWithTail elements

let createGraph edgeWeight edges : Graph =
   edges |> List.choose (fun (u, v) -> match edgeWeight (u, v) with
                                       | Some w -> Some (u, v, w)
                                       | _ -> None)

let private getUnvisitedVertexOnConnectedEdge visited (u, v, _) =
   match Set.contains u visited, Set.contains v visited with
   | true, false -> Some v
   | false, true -> Some u
   | _ -> None

let private findNextConnectedEdgeWithMinimalWeight visited sortedEdges =
   let chooseEdge edge =
      match getUnvisitedVertexOnConnectedEdge visited edge with
      | Some vertex -> Some (edge, vertex)
      | _ -> None
   List.takeOut chooseEdge sortedEdges

let minimalSpanigTree (edges: Graph) : Graph option = 
   if edges |> List.isEmpty then None else

   let sortedEdges = edges |> List.sortBy (fun (u, v, w) -> w)
   let first = match edges.Head with | (u, _, _) -> u
   let countAllVertices = edges
                          |> Seq.collect (fun (u, v, w) -> [u; v]) 
                          |> Seq.distinct 
                          |> Seq.length

   let rec mst visited leftToVisit solution sortedEdges =
      if leftToVisit = 0 
      then Some solution
      else match findNextConnectedEdgeWithMinimalWeight visited sortedEdges with
                 | Some ((edge, vertex), restOfEdges) -> mst (visited.Add vertex) (leftToVisit - 1) (edge::solution) restOfEdges
                 | _ -> None

   mst (set [first]) (countAllVertices - 1) [] sortedEdges

let private getBestEdgeToGraph edgeWeight (edges: Graph) (terminal: Vertex) =
   let possibleEdges = edges |> Seq.collect (fun (u, v, _) -> [u; v])
                             |> Seq.distinct
                             |> Seq.choose (fun vertex -> match edgeWeight (terminal, vertex) with
                                                          | Some w -> Some (terminal, vertex, w)
                                                          | _ -> None)
                             |> List.ofSeq
   if possibleEdges.Length = 0
   then None
   else possibleEdges |> List.minBy (fun (_, _, w) -> w) 
                      |> Some

let addTerminalsToGraph edgeWeight (terminals: Vertex list) (edges: Graph): Graph option =
   let getBestEdgeToGraph = getBestEdgeToGraph edgeWeight edges
   let bestEdges = terminals |> List.map getBestEdgeToGraph

   if bestEdges |> List.tryFind Option.isNone |> Option.isSome
   then None
   else bestEdges |> List.choose id
                  |> List.append edges
                  |> Some

let getDeadends terminals (edges: Graph) = 
   let isTerminal x = List.contains x terminals

   edges |> Seq.collect (fun (u, v, w) -> [u; v])
         |> Seq.filter (fun x -> not (isTerminal x))
         |> Seq.countBy id
         |> Seq.choose (fun (key, count) -> if count = 1 then Some key else None)
         |> List.ofSeq

let reduceGraph (getDeadends: Graph -> Vertex list) (edges: Graph) =
   let notGoingToDeadEnd deadends (u, v, w) =
      not (List.contains u deadends || List.contains v deadends)

   let rec reduce edges =
      let deadends = getDeadends edges
      if deadends.Length > 0
      then reduce (edges |> List.filter (notGoingToDeadEnd deadends))
      else edges
   reduce edges

let private steinerTreeAlgorithm combinationsWithoutRepetition2ofN createGraph addTerminalsToGraph reduceGraph minimalSpanigTree =
   combinationsWithoutRepetition2ofN
   >> createGraph
   >> addTerminalsToGraph
   >> Option.map reduceGraph
   >> Option.bind minimalSpanigTree

let builder edgeWeight terminals: (Vertex list -> SteinerTree) =
   steinerTreeAlgorithm
      combinationsWithoutRepetition2ofN 
      (createGraph edgeWeight)
      (addTerminalsToGraph edgeWeight terminals)
      (reduceGraph (getDeadends terminals))
      minimalSpanigTree

let cost (steinerTree: SteinerTree) : Weight = 
   steinerTree |> Option.map (List.fold (fun acc (_, _, w) -> acc + w) 0.0)
