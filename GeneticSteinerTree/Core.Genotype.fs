module GeneticSteinerTree.Core.Genotype

/// activator - returns true if vertex should be active gene otherwise false
let createGenotype activator genes =
   let createGene gene =
      if activator gene 
      then Active gene 
      else Inactive gene
   genes |> Seq.map createGene |> List.ofSeq |> Genotype

let crossGenotypes crossPoint (Genotype firstGenes) (Genotype secondGenes) =
   [
      (List.take crossPoint firstGenes) @ (List.skip crossPoint secondGenes) |> Genotype
      (List.take crossPoint secondGenes) @ (List.skip crossPoint firstGenes) |> Genotype
   ]

/// mutator - returns true if gene should be reverted, otherwise false
let mutateGenotype mutator (Genotype genes) = 
   let revert = function
      | Active v -> Inactive v
      | Inactive v -> Active v

   let mutate gene =
      if mutator gene then revert gene else gene

   genes |> List.map mutate |> Genotype
