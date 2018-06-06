module GeneticSteinerTree.Core.PopulationProcessing.Population
open GeneticSteinerTree.Core.PopulationOperators
open GeneticSteinerTree.Core
open Data
open Genotype
open GeneticSteinerTree.Core.Graph
open Roulette
open Microsoft.FSharp

let create geneActivator genes size =
   let alignToEven x = if x % 2 = 0 then x else x + 1
   let size = if size < 2 then 2 else (alignToEven size)
   let genotypes = List.init size (fun _ -> Genotype.create geneActivator genes)
   Population genotypes

let rank genotypeCost (Population genotypes) =
   genotypes 
   |> List.map (fun genotype -> genotype, genotypeCost genotype) 
   |> RankedPopulation

let bestForWorst (RankedPopulation current) (RankedPopulation next) =
   List.append current next
   |> List.sortBy (fun (_, w) -> match w with
                                 | Some v -> v
                                 | _ -> Core.float.MaxValue)
   |> List.take (List.length current)

let next ranker algorithm bestsSelector population =
   let ranked = population |> ranker
   ranked 
   |> algorithm 
   |> bestsSelector ranked
   |> List.map (fun (genotype, _) -> genotype) 
   |> Population

let evaluate next iterations population =
   let final = seq { 1 .. iterations }
               |> Seq.fold (fun acc _ -> next acc) population
   final

module Factory = 
   let private createRank edgeWeight terminals =
      let buildSteinerTree = SteinerTree.builder edgeWeight terminals
      let genotypeCost = Genotype.activeGenes >> buildSteinerTree >> SteinerTree.cost
      rank genotypeCost

   let private createGeneticAlgorithm randNext rank =
      let runRoulette = Roulette.Factory.createRun randNext 100000
      let select = Population.selectParents runRoulette
      let cross = Population.Factory.createCross 0.95 randNext
      let mutate = Population.Factory.createMutate 0.05 randNext
      select >> cross >> mutate >> rank

   let createEvaluate randNext edgeWeight terminals =
      let rank = createRank edgeWeight terminals
      let geneticAlgorithm = createGeneticAlgorithm randNext rank
      let next = next rank geneticAlgorithm bestForWorst
      evaluate next
