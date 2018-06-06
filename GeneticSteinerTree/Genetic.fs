module GeneticSteinerTree.Genetic
open GeneticSteinerTree.Core.Data
open GeneticSteinerTree.Core.PopulationProcessing

module private Utilities =
   let createRandNext () =
      let rnd = System.Random()
      let randNext range = rnd.Next(range)
      randNext

   let parcentProbabilityOfGeneActivation countForks countTerminals = 
      let probability = min ((float)countTerminals / (float)(countForks + countTerminals)) 0.5
      let parcent = (int)(probability * 100.0 + 0.5)
      parcent

[<CompiledName("CreatePopulation")>]
let createPopulation populationSize (vertices: Vertex seq) countTerminals = 
   let randNext = Utilities.createRandNext ()
   let prob = Utilities.parcentProbabilityOfGeneActivation (vertices |> Seq.length) countTerminals
   let geneActivator _ = randNext(100) < prob
   Population.create geneActivator vertices populationSize

let calculate (getEdgeCost: Vertex * Vertex -> Weight) (forks: Vertex seq) (terminals: Vertex seq) iterations =
   ()

//[<CompiledName("EvaluatePopulation")>]
//let evaluatePopulation ranker iterations population =
//   Population.evaluatePopulation (createRandNext ()) ranker iterations population   

//[<CompiledName("GetTopSolution")>]
//let getTopSolution population =
//   Population.getTopSolution population
