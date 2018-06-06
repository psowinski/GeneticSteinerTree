module GeneticSteinerTree.Core.SteinerTree
open GeneticSteinerTree.Extensions
open GeneticSteinerTree.Core
open Data

let private steinerTreeAlgorithm createEdges createGraph addTerminalsToGraph reduceGraph minimalSpanigTree =
   createEdges
   >> createGraph
   >> addTerminalsToGraph
   >> Option.map reduceGraph
   >> Option.bind minimalSpanigTree

let builder edgeWeight terminals: (Vertex list -> SteinerTree) =
   steinerTreeAlgorithm
      Graph.generateEdges
      (Graph.create edgeWeight)
      (Graph.addTerminals edgeWeight terminals)
      (Graph.Factory.createReduce terminals)
      MinimalSpaningTree.create

let cost (tree: SteinerTree) : Weight = 
   let folder acc (_, _, w) = acc + w
   tree |> Option.map (List.fold folder 0.0)
