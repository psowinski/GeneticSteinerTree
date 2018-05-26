module GeneticSteinerTree.Core.Population.Operators
open GeneticSteinerTree.Core.Data
open GeneticSteinerTree.Core.Genotype

let selectParents selector population = 
   population |> selector
              |> List.chunkBySize 2
              |> List.map (fun x -> x.[0], x.[1])

let crossPopulation randNext (parents: (Genotype * Genotype) list) = 
   let genotypeLength = (fst parents.Head) |> fun (Genotype genes) -> List.length genes
   let crossPoint () = randNext(genotypeLength) + 1

   let crossedPopulation =
      parents  |> List.collect (fun pair -> crossGenotypes (crossPoint ()) (fst pair) (snd pair))
               |> Population
   crossedPopulation

let mutatePopulation randNext (Population genotypes) = 
   let mutator _ = randNext(1000) < 5 //0.5%
   let mutatedPopulation =
      genotypes |> List.map (mutateGenotype mutator) |> Population
   mutatedPopulation
