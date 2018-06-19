module GeneticSteinerTreeTests.Core.PopulationProcessingShould
open GeneticSteinerTree.Core.Data
open GeneticSteinerTree.Core.PopulationProcessing
open Xunit

[<Fact>]
let ``Create population`` () = 
   let expected = Population [Genotype [Active "a"]; Genotype [Active "a"]]
   let actual = Population.create (fun _ -> Genotype [Active "a"]) ["x"] 2
   Assert.Equal(expected, actual)

[<Theory>]
[<InlineData(-1, 2)>]
[<InlineData(0, 2)>]
[<InlineData(4, 4)>]
[<InlineData(7, 8)>]
let ``Round population size to even natural number, greather then zero`` size expected = 
   let actual = Population.create (fun _ -> Genotype [Active "a"]) ["x"] size
                |> Population.length
   Assert.Equal(expected, actual)

[<Fact>]
let ``Rank population`` () = 
   let expected = RankedPopulation [
                     Genotype [Active "a"], Some 9.0;
                     Genotype [Inactive "a"], Some 8.0]

   let population = Population [Genotype [Active "a"]; Genotype [Inactive "a"]]
   
   let mutable idx = 10.0
   let getCost _ = 
      idx <- idx - 1.0
      Some idx

   let actual = Population.rank getCost population
   Assert.Equal(expected, actual)

[<Fact>]
let ``Replace worst genotypes by best from other population`` () = 
   let population1 = RankedPopulation [
                        Genotype [Active "a"], Some 5.0;
                        Genotype [Active "b"], Some 1.0]
   let population2 = RankedPopulation [
                        Genotype [Active "x"], Some 3.0;
                        Genotype [Active "y"], Some 4.0]

   let expected = [Genotype [Active "b"], Some 1.0;
                   Genotype [Active "x"], Some 3.0]

   let actual = Population.bestForWorst population1 population2
   Assert.Equal<(Genotype * Weight) list>(expected, actual)
