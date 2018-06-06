module GeneticSteinerTree.Core.MinimalSpaningTree
open GeneticSteinerTree.Extensions
open GeneticSteinerTree.Core.Data

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

let create (edges: Graph) : SteinerTree = 
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
