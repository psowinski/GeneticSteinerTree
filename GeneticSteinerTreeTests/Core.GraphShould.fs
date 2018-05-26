module GeneticSteinerTreeTests.Core.GraphShould
open GeneticSteinerTree.Core.Data
open GeneticSteinerTree.Core.Graph
open Xunit

[<Fact>]
let ``Calculate comination without repetition 2 of N as edges`` () = 
   let expected = [(1, 2); (1, 3); (2, 3)]
   let actual = combinationsWithoutRepetition2ofN [1..3] |> List.sort
   Assert.Equal<(int * int) list>(expected, actual)

[<Fact>]
let ``Create graph should add weight to edges`` () = 
   let edges = [("1", "2"); ("1", "3"); ("2", "3")]
   let expected = [("1", "2", 5.0); ("1", "3", 5.0); ("2", "3", 5.0)]
   let actual = createGraph (fun (u, v) -> Some 5.0) edges
   Assert.Equal<Graph>(expected, actual)

[<Fact>]
let ``Create graph should remove edges without path`` () = 
   let edges = [("1", "2"); ("1", "3"); ("2", "3")]
   let expected = [("2", "3", 0.0)]
   let getEdgeWeight (u, v) = if u <> "1" then Some 0.0 else None
   let actual = createGraph getEdgeWeight edges
   Assert.Equal<Graph>(expected, actual)

[<Fact>]
let ``Create minimal spaning tree`` () = 
   let graph = ["1", "2", 0.0;
                "2", "4", 1.0; 
                "2", "5", 1.0; 
                "2", "3", 0.0;
                "3", "4", 0.0;
                "3", "5", 0.1]
   let expected = ["1", "2", 0.0;
                   "2", "3", 0.0;
                   "3", "4", 0.0;
                   "3", "5", 0.1]
   let actual = minimalSpanigTree graph |> Option.get |> List.sort
   Assert.Equal<Graph>(expected, actual);

[<Fact>]
let ``Return None for minimal spaning tree if graph is not connected`` () = 
   let graph = ["1", "2", 0.0;
                "3", "4", 0.0;
                "3", "5", 0.1]
   let actual = minimalSpanigTree graph
   Assert.Equal<Graph option>(None, actual);

[<Fact>]
let ``Return None for minimal spaning tree if graph is empty`` () = 
   let actual = minimalSpanigTree []
   Assert.Equal<Graph option>(None, actual);

[<Fact>]
let ``Get deadends sould return not terminal deadends`` () = 
   let graph = ["1", "2", 0.0;
                "1", "3", 0.0;
                "2", "4", 0.0]
   let expected = ["3"; "4"]
   let actual = getDeadends ["1"; "2"] graph |> List.sort
   Assert.Equal<Vertex list>(expected, actual);

[<Fact>]
let ``Reduce graph should remove deadends`` () = 
   let mutable terminals = ["3"; "4"]
   let getDeadends _ =  match terminals with
                        | [] -> []
                        | x::xs -> terminals <- xs; [x]

   let graph = ["1", "2", 0.0;
                "1", "3", 0.0;
                "2", "4", 0.0]
   let expected = ["1", "2", 0.0]
   let actual = reduceGraph getDeadends graph
   Assert.Equal<Graph>(expected, actual);

[<Fact>]
let ``Reduce graph should do noting if there is no deadends`` () = 
   let graph = ["1", "2", 0.0;
                "1", "3", 0.0;
                "2", "4", 0.0]
   let actual = reduceGraph (fun _ -> []) graph
   Assert.Equal<Graph>(graph, actual);

[<Fact>]
let ``Reduce graph should do nothing on empty graph`` () = 
   let actual = reduceGraph (fun _ -> []) []
   Assert.Equal<Graph>([], actual);

[<Fact>]
let ``Add terminbals to graph by shortest path`` () = 
   let getEdgeWeight edge =
      match edge with
      | "3", "1" -> Some 1.0
      | "3", "2" -> Some 2.0
      | "4", "1" -> Some 2.0
      | "4", "2" -> Some 1.0
      | _ -> None

   let actual = addTerminalsToGraph getEdgeWeight ["3"; "4"] ["1", "2", 0.0]
                |> Option.get
                |> List.sort
   let expected = ["1", "2", 0.0;
                   "3", "1", 1.0;
                   "4", "2", 1.0]
   Assert.Equal<Graph>(expected, actual);

[<Fact>]
let ``Add terminals to graph by shortest existing path`` () = 
   let getEdgeWeight edge =
      match edge with
      | "3", "2" -> Some 2.0
      | _ -> None

   let actual = addTerminalsToGraph getEdgeWeight ["3"] ["1", "2", 0.0]
                |> Option.get
                |> List.sort
   let expected = ["1", "2", 0.0;
                   "3", "2", 2.0]
   Assert.Equal<Graph>(expected, actual);

[<Fact>]
let ``Add terminals to graph when no path`` () = 
   let actual = addTerminalsToGraph (fun _ -> None) ["3"] ["1", "2", 0.0]
   Assert.Equal<Graph option>(None, actual);