module GeneticSteinerTree.Core.Genotype

let createGenotype canPassFork forks =
   let createGene fork =
      if canPassFork fork then Active fork else Inactive fork
   forks |> Seq.map createGene |> List.ofSeq |> Genotype

let geneCrossing crossPoint (Genotype firstGenes) (Genotype secondGenes) =
   [
      (List.take crossPoint firstGenes) @ (List.skip crossPoint secondGenes) |> Genotype
      (List.take crossPoint secondGenes) @ (List.skip crossPoint firstGenes) |> Genotype
   ]

let geneMutation (mutateNow: unit -> bool) (Genotype genes) = 
   let revert = function
      | Active v -> Inactive v
      | Inactive v -> Active v

   let mutate gene =
      if (mutateNow ()) then revert gene  else gene

   genes |> List.map mutate |> Genotype

let getGenotypeIdentifiers (Genotype genes) =
   let getGeneIdentifier gene =
      match gene with
      | Active identifier -> identifier
      | Inactive identifier -> identifier
   genes |> Seq.map getGeneIdentifier
