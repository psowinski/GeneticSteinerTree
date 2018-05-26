module GeneticSteinerTree.Genetic
open GeneticSteinerTree.Core
open Genotype
open Population.Features

let private createRandNext () =
   let rnd = System.Random()
   let randNext range = rnd.Next(range)
   randNext

let private calculateForkPassProbability countForks countTerminals = 
   let probability = min ((float)countTerminals / (float)(countForks + countTerminals)) 0.5
   let parcent = (int)(probability * 100.0 + 0.5)
   parcent

let calculate (getEdgeCost: Vertex * Vertex -> Weight) (forks: Vertex seq) (terminals: Vertex seq) iterations =
   ()

[<CompiledName("CreatePopulation")>]
let createPopulation populationSize (forks: Vertex seq) countTerminals = 
   let randNext = createRandNext ()
   let prob = calculateForkPassProbability (forks |> Seq.length) countTerminals
   let canPassForkRandom _ = randNext(100) < prob
   createPopulation canPassForkRandom populationSize forks

//[<CompiledName("EvaluatePopulation")>]
//let evaluatePopulation ranker iterations population =
//   Population.evaluatePopulation (createRandNext ()) ranker iterations population   

//[<CompiledName("GetTopSolution")>]
//let getTopSolution population =
//   Population.getTopSolution population
