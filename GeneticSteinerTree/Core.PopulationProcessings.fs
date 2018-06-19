module GeneticSteinerTree.Core.PopulationProcessing.Population
open GeneticSteinerTree.Core.PopulationOperators
open GeneticSteinerTree.Core
open Data
open Genotype
open GeneticSteinerTree.Core.Graph
open Roulette
open Microsoft.FSharp

let create createGenotype genes size =
   let alignToEven x = if x % 2 = 0 then x else x + 1
   let size = if size < 2 then 2 else (alignToEven size)
   let genotypes = List.init size (fun _ -> createGenotype genes)
   Population genotypes

let rank genotypeCost (Population genotypes) =
   genotypes 
   |> List.map (fun genotype -> genotype, genotypeCost genotype) 
   |> RankedPopulation

let private weightToFloat (_, w) = match w with
                                   | Some v -> v
                                   | _ -> Core.float.MaxValue

let bestForWorst (RankedPopulation current) (RankedPopulation next) =
   List.append current next
   |> List.sortBy weightToFloat
   |> List.take (List.length current)

let next algorithm selector rankedPopulation =
   rankedPopulation
   |> algorithm 
   |> selector rankedPopulation
   |> RankedPopulation

let evaluate next iterations population =
   let final = seq { 1 .. iterations }
               |> Seq.fold (fun acc _ -> next acc) population
   final

let best rankdPopulation =
   let best = rankdPopulation
              |> (fun (RankedPopulation list) -> list |> List.minBy weightToFloat) 
              |> (fun (genotype, _) -> genotype)  
   best

let length (Population list) =
   List.length list

module Factory = 
   let createCreate geneActivator =
      create (Genotype.create geneActivator)

   let createRanker edgeWeight terminals =
      let buildSteinerTree = SteinerTree.builder edgeWeight terminals
      let genotypeCost = Genotype.activeGenes >> buildSteinerTree >> SteinerTree.cost
      rank genotypeCost

   let private createGeneticAlgorithm randNext =
      let runRoulette = Roulette.Factory.createRun randNext 100000
      let select = Population.selectParents runRoulette
      let cross = Population.Factory.createCross 0.95 randNext
      let mutate = Population.Factory.createMutate 0.05 randNext
      select >> cross >> mutate

   let createEvaluate randNext ranker =
      let algorithm = createGeneticAlgorithm randNext >> ranker
      let next = next algorithm bestForWorst
      evaluate next
