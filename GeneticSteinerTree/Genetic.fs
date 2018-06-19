module GeneticSteinerTree.Genetic
open GeneticSteinerTree.Core.Data
open GeneticSteinerTree.Core.PopulationProcessing
open GeneticSteinerTree.Core

type SteinerTreeProblem = 
| InitialProblem of InitialData
| ProcessedProblem of ProcessedData
and InitialData = {
   population: Population
   terminals: Vertex list
   edgeWeight: Vertex * Vertex -> Weight
}
and ProcessedData = {
   ranked: RankedPopulation
   terminals: Vertex list
   edgeWeight: Vertex * Vertex -> Weight
}

module Randomizer =
   let createRandNext () =
      let rnd = System.Random()
      let randNext range = rnd.Next(range)
      randNext

module GeneticSolver = 
   let private rankPopulation ranker problem = 
      match problem with
      | InitialProblem { population = population } -> population |> ranker
      | ProcessedProblem { ranked = ranked } -> ranked

   let private commonData (problem: SteinerTreeProblem) =
      match problem with
      | InitialProblem { terminals = terminals; edgeWeight = edgeWeight } -> edgeWeight, terminals
      | ProcessedProblem { terminals = terminals; edgeWeight = edgeWeight } -> edgeWeight, terminals

   let private processedData problem =
      match problem with
      | InitialProblem _ -> None
      | ProcessedProblem data -> Some data

   let private parcentProbabilityOfGeneActivation countForks countTerminals = 
      let probability = min ((float)countTerminals / (float)(countForks + countTerminals)) 0.5
      let parcent = (int)(probability * 100.0 + 0.5)
      parcent

   let createGeneActivator randNext countForks countTerminals =
      let prob = parcentProbabilityOfGeneActivation countForks countTerminals
      let geneActivator _ = randNext(100) < prob
      geneActivator

   let createProblem createPopulation populationSize edgeWeight vertices terminals =
      let population = createPopulation vertices populationSize
      InitialProblem { 
         population = population;
         terminals = terminals |> List.ofSeq;
         edgeWeight = edgeWeight }

   let calculate createRanker createEvaluate iterations problem =
      let (edgeWeight, terminals) = commonData problem
      let ranker = createRanker edgeWeight terminals
      let rankedPopulation = rankPopulation ranker problem

      let evaluate = createEvaluate (Randomizer.createRandNext ()) ranker
      let newRanked = evaluate iterations rankedPopulation

      ProcessedProblem { ranked = newRanked; terminals = terminals; edgeWeight = edgeWeight }

   let solution toBestEdges problem =
      problem |> processedData
              |> Option.bind (fun { edgeWeight = edgeWeight; terminals = terminals; ranked = ranked } -> 
                              toBestEdges edgeWeight terminals ranked)

[<CompiledName("CreateProblem")>]
let create populationSize (edgeWeight: Vertex * Vertex -> Weight) (intersections: Vertex seq) (terminals: Vertex seq) =
   let geneActivator = GeneticSolver.createGeneActivator (Randomizer.createRandNext ()) (Seq.length intersections) (Seq.length terminals)
   let createPopulation = Population.Factory.createCreate geneActivator 
   let createProblem' = GeneticSolver.createProblem createPopulation
   createProblem' populationSize edgeWeight intersections terminals

[<CompiledName("CalculateProblem")>]
let calculate iterations (problem: SteinerTreeProblem) =
   let calculate' = GeneticSolver.calculate Population.Factory.createRanker Population.Factory.createEvaluate
   calculate' iterations problem

[<CompiledName("GetProblemSolution")>]
let solution (problem: SteinerTreeProblem) =
   let toBestEdges edgeWeight terminals = Population.best
                                          >> Genotype.activeGenes
                                          >> (SteinerTree.builder edgeWeight terminals)
                                          >> (Option.map Graph.edges)
   GeneticSolver.solution toBestEdges problem
