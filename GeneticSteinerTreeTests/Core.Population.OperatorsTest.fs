﻿module GeneticSteinerTreeTests.Core.Population.OperatorsTest
open GeneticSteinerTree.Core.Data
open GeneticSteinerTree.Core.Population.Operators
open Xunit

[<Fact>]
let ``Select parents by selector`` () = 
   let expected = [Genotype [Active "a"], Genotype [Active "c"]]
   let pop = Population [ Genotype [Active "a"];
                          Genotype [Active "b"];
                          Genotype [Active "c"];
                          Genotype [Active "d"]]

   let actual = selectParents (fun (Population list) -> [list.[0]; list.[2]]) pop
   Assert.Equal<(Genotype * Genotype) list>(expected, actual)

[<Fact>]
let ``Cross population if it hits the probability`` () = 
   let parents = [ Genotype [], Genotype []]
   let genotypeAbc = Genotype [Active "abc"]
   let corssPopulation = createCrossPopulation (fun _ _ _ -> [genotypeAbc])

   let actual = corssPopulation 0.5 (fun x -> x / 2 - 1) parents
   let expected = Population [genotypeAbc]

   Assert.Equal<Population>(expected, actual)

[<Fact>]
let ``Not cross population if its out of the probability`` () = 
   let (g1, g2) =  Genotype [Active "a"],
                   Genotype [Active "x"]

   let corssPopulation = createCrossPopulation (fun _ _ _ -> [])

   let actual = corssPopulation 0.5 (fun x -> x / 2 + 1) [g1, g2]
   let expected = Population [g1; g2]

   Assert.Equal<Population>(expected, actual)

[<Fact>]
let ``Mutate population if it hits the probability`` () = 
   let population = Population [Genotype []]
   let genotypeAbc = Genotype [Active "abc"]
   let mutatePopulation = createMutatePopulation (fun _ _ -> genotypeAbc)

   let actual = mutatePopulation 0.5 (fun x -> x / 2 - 1) population
   let expected = Population [genotypeAbc]

   Assert.Equal<Population>(expected, actual)

[<Fact>]
let ``Not mutate population if its out of the probability`` () = 
   let population = Population [Genotype [Active "xyz"]]
   let mutatePopulation = createMutatePopulation (fun _ _ -> Genotype [])

   let actual = mutatePopulation 0.5 (fun x -> x / 2 + 1) population

   Assert.Equal<Population>(population, actual)