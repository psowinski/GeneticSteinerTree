module GeneticSteinerTreeTests.Core.MinimalSpaningTreeShould
open GeneticSteinerTree.Core.Data
open GeneticSteinerTree.Core
open Xunit

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
   let actual = MinimalSpaningTree.create graph |> Option.get |> List.sort
   Assert.Equal<Graph>(expected, actual);

[<Fact>]
let ``Return None for minimal spaning tree if graph is not connected`` () = 
   let graph = ["1", "2", 0.0;
                "3", "4", 0.0;
                "3", "5", 0.1]
   let actual = MinimalSpaningTree.create graph
   Assert.Equal<SteinerTree>(None, actual);

[<Fact>]
let ``Return None for minimal spaning tree if graph is empty`` () = 
   let actual = MinimalSpaningTree.create []
   Assert.Equal<SteinerTree>(None, actual);