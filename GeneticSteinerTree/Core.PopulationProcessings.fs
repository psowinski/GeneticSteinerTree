module GeneticSteinerTree.Core.PopulationProcessing.Population
open GeneticSteinerTree.Core.PopulationOperators
open GeneticSteinerTree.Core
open Data
open Genotype
open Graph
open Roulette
open Microsoft.FSharp

let createPopulation geneActivator genes size =
   let alignToEven x = if x % 2 = 0 then x else x + 1
   let size = if size < 2 then 2 else (alignToEven size)
   let genotypes = List.init size (fun _ -> Genotype.create geneActivator genes)
   Population genotypes

let getGenotypeCost getSteinerTree (Genotype genes): Weight = 
   let cost = genes |> List.choose (function | Active v -> Some v | _ -> None)
                    |> getSteinerTree
                    |> Option.map (List.fold (fun acc (_, _, w) -> acc + w) 0.0)
   cost

let rankPopulation getGenotypeCost (Population genotypes) =
   genotypes 
   |> List.map (fun genotype -> genotype, getGenotypeCost genotype) 
   |> RankedPopulation

let bestForWorst (RankedPopulation current) (RankedPopulation next) =
   List.append current next
   |> List.sortBy (fun (g, w) -> match w with
                                 | Some v -> v
                                 | _ -> Core.float.MaxValue)
   |> List.take (List.length current)

let geneticAlgorithm select cross mutate rank =
   select >> cross >> mutate >> rank

let nextPopulation rankPopulation algorithm selectBests population =
   let ranked = rankPopulation population
   ranked 
   |> algorithm 
   |> selectBests ranked
   |> List.map (fun (g, _) -> g) 
   |> Population

let evaluatePopulation nextPopulation iterations population =
   let final = seq { 1 .. iterations }
               |> Seq.fold (fun acc _ -> nextPopulation acc) population
   final

module Factory = 
   let createEvaluatePopulation randNext getEdgeWeight terminals =
      let getSteinerTree = Factory.createGetSteinerTree getEdgeWeight terminals
      let getCost = getGenotypeCost getSteinerTree
      let runRoulette = Roulette.Factory.createRun randNext 100000
      let select = Population.selectParents runRoulette
      let cross = Population.Factory.createCross 0.95 randNext
      let mutate = Population.Factory.createMutate 0.05 randNext
      let rank = rankPopulation getCost
      let algorithm = geneticAlgorithm select cross mutate rank
      let next = nextPopulation rank algorithm bestForWorst
      evaluatePopulation next
