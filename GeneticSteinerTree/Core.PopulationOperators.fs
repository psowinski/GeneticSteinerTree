﻿module GeneticSteinerTree.Core.PopulationOperators.Population
open GeneticSteinerTree.Core

module RankedPopulation =
   let length (RankedPopulation list) = List.length list

let selectParents selector population = 
   selector population (RankedPopulation.length population)
   |> List.chunkBySize 2
   |> List.map (fun x -> x.[0], x.[1])

let cross crosser probability randNext parents = 
   let probability = int(probability * 10000.0)
   let genotypeLength = parents |> List.head
                                |> fst
                                |> Genotype.length
   let crossPoint () = randNext (genotypeLength - 1) + 1
   let crossing (genotype1, genotype2) = 
      if randNext 10000 < probability
      then crosser (crossPoint ()) genotype1 genotype2
      else [genotype1; genotype2]

   let crossedPopulation =
      parents  |> List.collect crossing
               |> Population
   crossedPopulation

let mutate mutator probability randNext (Population genotypes) = 
   let probability = int(probability * 10000.0)
   let genotypeLength = genotypes |> List.head |> Genotype.length
   let mutation genotype = 
      if randNext 10000 < probability
      then mutator (randNext genotypeLength) genotype
      else genotype

   let mutatedPopulation =
      genotypes |> List.map mutation |> Population
   mutatedPopulation

module Factory =
   let createCross probability randNext parents = 
      cross Genotype.cross probability randNext parents

   let createMutate probability randNext population = 
      mutate Genotype.mutate probability randNext population
   