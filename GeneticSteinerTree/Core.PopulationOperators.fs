module GeneticSteinerTree.Core.PopulationOperators.Population
open GeneticSteinerTree.Core

let selectParents selector population = 
   population |> selector
              |> List.chunkBySize 2
              |> List.map (fun x -> x.[0], x.[1])

let createCross crosser probability randNext parents = 
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

let cross probability randNext parents = 
   createCross Genotype.cross probability randNext parents

let createMutate mutator probability randNext (Population genotypes) = 
   let probability = int(probability * 10000.0)
   let genotypeLength = genotypes |> List.head |> Genotype.length
   let mutation genotype = 
      if randNext 10000 < probability
      then mutator (randNext genotypeLength) genotype
      else genotype

   let mutatedPopulation =
      genotypes |> List.map mutation |> Population
   mutatedPopulation

let mutate probability randNext population = 
   createMutate Genotype.mutate probability randNext population