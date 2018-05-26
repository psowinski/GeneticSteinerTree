module GeneticSteinerTree.Core.Population.Features
open GeneticSteinerTree.Core
open Data
open Genotype
open Graph
open Population.Roulette
open Population.Operators
open Microsoft.FSharp

let createPopulation canPassForkRnd populationSize forks =
   let alignToEven x = if x % 2 = 0 then x else x + 1
   let size = if populationSize < 2 then 2 else (alignToEven populationSize)
   let genotypes = List.init size (fun _ -> createGenotype canPassForkRnd forks)
   Population genotypes

let getGenotypeCost getEdgeWeight (terminals: Vertex list) (Genotype genes): Weight = 
   let cost = genes |> List.choose (function | Active v -> Some v | _ -> None)
                    |> getSteinerTreeByMinimalSpanningTree getEdgeWeight terminals
                    |> Option.map (List.fold (fun acc (u, v, w) -> acc + w) 0.0)
   cost

let private rankPopulation getGenotypeCost (Population genotypes) =
   genotypes 
   |> List.map (fun genotype -> genotype, getGenotypeCost genotype) 
   |> RankedPopulation

let private bestForWorst (RankedPopulation p1) (RankedPopulation p2) =
   List.append p1 p2
   |> List.sortBy (fun (g, w) -> match w with
                                 | Some v -> v
                                 | _ -> Core.float.MaxValue)
   |> List.take (List.length p1)

let nextPopulation getGenotypeCost selectParents crossPopulation mutatePopulation population =
   let rankPopulation = rankPopulation getGenotypeCost
   let current = rankPopulation population
   let next = selectParents >> crossPopulation >> mutatePopulation >> rankPopulation >> bestForWorst current

   current 
   |> next 
   |> List.map (fun (g, w) -> g) 
   |> Population

let evaluatePopulation nextPopulation iterations population =
   let rec processPopulation iteration population =
      if iteration > 0
      then processPopulation (iteration - 1) (nextPopulation population)
      else population
   processPopulation iterations population

let evaluatePopulationFactory randNext getEdgeWeight terminals =
   let cost = getGenotypeCost getEdgeWeight terminals
   let select = selectParents (rouletteSelection 100000 randNext)
   let cross = crossPopulation 0.95 randNext
   let mutate = mutatePopulation 0.05 randNext
   let next = nextPopulation cost select cross mutate
   evaluatePopulation next
