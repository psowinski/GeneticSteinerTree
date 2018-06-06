module GeneticSteinerTreeTests.Core.GraphShould
open GeneticSteinerTree.Core.Data
open GeneticSteinerTree.Core
open Xunit

[<Fact>]
let ``Calculate comination without repetition 2 of N as edges`` () = 
   let expected = [(1, 2); (1, 3); (2, 3)]
   let actual = Graph.generateEdges [1..3] |> List.sort
   Assert.Equal<(int * int) list>(expected, actual)

[<Fact>]
let ``Create graph should add weight to edges`` () = 
   let edges = [("1", "2"); ("1", "3"); ("2", "3")]
   let expected = [("1", "2", 5.0); ("1", "3", 5.0); ("2", "3", 5.0)]
   let actual = Graph.create (fun (u, v) -> Some 5.0) edges
   Assert.Equal<Graph>(expected, actual)

[<Fact>]
let ``Create graph should remove edges without path`` () = 
   let edges = [("1", "2"); ("1", "3"); ("2", "3")]
   let expected = [("2", "3", 0.0)]
   let getEdgeWeight (u, v) = if u <> "1" then Some 0.0 else None
   let actual = Graph.create getEdgeWeight edges
   Assert.Equal<Graph>(expected, actual)

[<Fact>]
let ``Get deadends sould return not terminal deadends`` () = 
   let graph = ["1", "2", 0.0;
                "1", "3", 0.0;
                "2", "4", 0.0]
   let expected = ["3"; "4"]
   let actual = Graph.getDeadends ["1"; "2"] graph |> List.sort
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
   let actual = Graph.reduce getDeadends graph
   Assert.Equal<Graph>(expected, actual);

[<Fact>]
let ``Reduce graph should do noting if there is no deadends`` () = 
   let graph = ["1", "2", 0.0;
                "1", "3", 0.0;
                "2", "4", 0.0]
   let actual = Graph.reduce (fun _ -> []) graph
   Assert.Equal<Graph>(graph, actual);

[<Fact>]
let ``Reduce graph should do nothing on empty graph`` () = 
   let actual = Graph.reduce (fun _ -> []) []
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

   let actual = Graph.addTerminals getEdgeWeight ["3"; "4"] ["1", "2", 0.0]
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

   let actual = Graph.addTerminals getEdgeWeight ["3"] ["1", "2", 0.0]
                |> Option.get
                |> List.sort
   let expected = ["1", "2", 0.0;
                   "3", "2", 2.0]
   Assert.Equal<Graph>(expected, actual);

[<Fact>]
let ``Add terminals to graph when no path`` () = 
   let actual = Graph.addTerminals (fun _ -> None) ["3"] ["1", "2", 0.0]
   Assert.Equal<Graph option>(None, actual);