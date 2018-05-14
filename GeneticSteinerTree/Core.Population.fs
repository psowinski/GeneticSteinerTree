module GeneticSteinerTree.Core.Population
open Genotype

let createPopulation canPassFork populationSize forks =
   let alignToEven x = if x % 2 = 0 then x else x + 1
   let size = if populationSize < 2 then 2 else (alignToEven populationSize)

   { 1 .. size }
   |> Seq.map (fun _ -> createGenotype canPassFork forks)
   |> List.ofSeq 
   |> Population

let calculateForkPassProbability countForks countTerminals = 
   let probability = min ((float)countTerminals / (float)(countForks + countTerminals)) 0.5
   let parcent = (int)(probability * 100.0 + 0.5)
   parcent

let selectParents randNext (Population genotypes) = 
   genotypes |> List.sortBy (fun _ -> randNext(System.Int32.MaxValue))
             |> List.chunkBySize 2
             |> List.map (fun x -> x.[0], x.[1])

let crossPopulation randNext (parents: (Genotype * Genotype) list) = 
   let genotypeLength = (fst parents.Head) |> fun (Genotype genes) -> List.length genes
   let crossPoint () = randNext(genotypeLength) + 1

   let crossedPopulation =
      parents  |> List.collect (fun pair -> geneCrossing (crossPoint ()) (fst pair) (snd pair))
               |> Population
   crossedPopulation

let mutatePopulation randNext (Population genotypes) = 
   let mutateNow () = randNext(100) < 1
   let mutatedPopulation =
      genotypes |> List.map (geneMutation mutateNow) |> Population
   mutatedPopulation

let rankPopulation (ranker: RankingFunc) (Population genotypes) =
   let rankedPopulation =
      genotypes |> List.sortBy (fun (Genotype genes) -> ranker genes)
                |> Population
   rankedPopulation

let getTopSolution (Population genotypes)=
   genotypes |> List.head |> getGenotypeIdentifiers

let createNextPopulation randNext ranker population =
   population |> (selectParents randNext)
              |> (crossPopulation randNext)
              |> (mutatePopulation randNext)
              |> (rankPopulation ranker)

let evaluatePopulation randNext ranker iterations population =
   let createNextPopulation = createNextPopulation randNext ranker
   let rec processPopulation iterations population =
      if iterations > 0
      then processPopulation (iterations - 1) (createNextPopulation population)
      else population
   processPopulation iterations population