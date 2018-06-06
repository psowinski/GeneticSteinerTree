module GeneticSteinerTreeTests.Core.SteinerTreeShould
open GeneticSteinerTree.Core.Data
open GeneticSteinerTree.Core
open Xunit

[<Fact>]
let ``Calculate cost for existing graph`` () = 
   let graph = ["1", "2", 1.0;
                "3", "4", 2.0;
                "3", "5", 0.1]
   let actual = SteinerTree.cost (Some graph)
   Assert.Equal<Weight>(Some 3.1, actual);

[<Fact>]
let ``Calculate cost for nonexisting graph`` () = 
   let actual = SteinerTree.cost None
   Assert.Equal<Weight>(None, actual);
