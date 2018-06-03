module GeneticSteinerTree.Core.Roulette

let private weightToRange range (RankedPopulation rankedGenotypes) = 
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

type private Range = int
type private From = int
type private To = int
type Roulette<'T> = | Roulette of (('T * From * To) list) * Range

let create range (RankedPopulation rankedGenotypes) =
   let roundHeadToEndOfRange list = match list with
                                    | [] -> []
                                    | (g, f, t)::xs -> if t = range then list else (g, f, range)::xs

   let weightToRange = weightToRange range (RankedPopulation rankedGenotypes)

   let roulette = rankedGenotypes |> Seq.map (fun (g, w) -> g, weightToRange w)
                                  |> Seq.filter (fun (_, range) -> range > 0)
                                  |> Seq.fold (fun acc (g, range) -> 
                                                match acc with
                                                | [] -> [g, 0, range]
                                                | (_, _, prevRange)::_ -> (g, prevRange, prevRange + range)::acc)
                                               []
                                  |> List.ofSeq
                                  |> roundHeadToEndOfRange
   Roulette (roulette, range)

let select randNext count (Roulette (roulette, range)) =
   let runRoulette _ = 
      let winValue = randNext range
      List.find (fun (g, fromRange, toRange) -> fromRange <= winValue && winValue < toRange) roulette
      |> function | (g, _, _) -> g
   let selected = List.init count runRoulette
   selected

let run create select randNext range (RankedPopulation rankedGenotypes) =
   let count = List.length rankedGenotypes
   create range (RankedPopulation rankedGenotypes) 
   |> select randNext count

module Factory =
   let createRun randNext range rankedPopulation =
      run create select randNext range rankedPopulation
