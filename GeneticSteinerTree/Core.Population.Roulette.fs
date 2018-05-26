module GeneticSteinerTree.Core.Population.Roulette
open GeneticSteinerTree.Core.Data

let private createWeightToRange range (RankedPopulation rankedGenotypes) = 
   let totalWeight = 
      rankedGenotypes 
      |> Seq.fold (fun acc (_, w) -> acc + (function | Some w -> w | _ -> 0.0) w) 0.0

   let noneRange = 
      if totalWeight > 0.0 then 0
      else 
         let countNone = rankedGenotypes |> Seq.sumBy  (fun (_, w) -> match w with | None -> 1 | _ -> 0)
         if countNone = 0 then 0 else range / countNone

   let rangeUnit = if totalWeight = 0.0 then 0.0 else (float)(range - 1) / totalWeight

   function
   | Some w -> int (w * rangeUnit + 0.5) 
   | None -> noneRange

let createRoulette range (RankedPopulation rankedGenotypes) =
   let roundHeadToEndOfRange list = match list with
                                    | [] -> []
                                    | (g, f, t)::xs -> if t = range then list else (g, f, range)::xs

   let weightToRange = createWeightToRange range (RankedPopulation rankedGenotypes)

   let roulette = rankedGenotypes |> Seq.map (fun (g, w) -> g, weightToRange w)
                                  |> Seq.filter (fun (_, range) -> range > 0)
                                  |> Seq.fold (fun acc (g, range) -> 
                                                match acc with
                                                | [] -> [g, 0, range]
                                                | (_, _, prevRange)::_ -> (g, prevRange, prevRange + range)::acc)
                                               []
                                  |> List.ofSeq
                                  |> roundHeadToEndOfRange
   roulette

let rouletteSelection precision randNext (RankedPopulation rankedGenotypes) =
   if precision < 100 then failwith "Too low precision, should be >= 100"

   let populationSize = List.length rankedGenotypes
   let roulette = createRoulette precision (RankedPopulation rankedGenotypes)
   let runRoulette _ = 
      let winValue = randNext(precision)
      List.find (fun (g, fromRange, toRange) -> fromRange <= winValue && winValue < toRange) roulette
      |> function | (g, _, _) -> g

   let selected = List.init populationSize runRoulette
   selected
