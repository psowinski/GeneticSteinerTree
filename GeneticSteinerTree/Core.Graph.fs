module GeneticSteinerTree.Core.Graph
open GeneticSteinerTree.Extensions
open GeneticSteinerTree.Core.Data

///combinations without repetition 2 of N
let generateEdges vertices =
   let pairHeadWithTail = function
      | [] -> Seq.empty
      | x::xs -> xs |> Seq.map (fun y -> x, y)
   List.collectTail pairHeadWithTail vertices

let create edgeWeight edges : Graph =
   edges |> List.choose (fun (u, v) -> match edgeWeight (u, v) with
                                       | Some w -> Some (u, v, w)
                                       | _ -> None)

let private bestEdgeToGraph edgeWeight (edges: Graph) (terminal: Vertex) =
   let possibleEdges = edges |> Seq.collect (fun (u, v, _) -> [u; v])
                             |> Seq.distinct
                             |> Seq.choose (fun vertex -> match edgeWeight (terminal, vertex) with
                                                          | Some w -> Some (terminal, vertex, w)
                                                          | _ -> None)
                             |> List.ofSeq
   if possibleEdges.Length = 0
   then None
   else possibleEdges |> List.minBy (fun (_, _, w) -> w) 
                      |> Some

let addTerminals edgeWeight (terminals: Vertex list) (edges: Graph): Graph option =
   let bestEdgeToGraph = bestEdgeToGraph edgeWeight edges
   let bestEdges = terminals |> List.map bestEdgeToGraph

   if bestEdges |> List.tryFind Option.isNone |> Option.isSome
   then None
   else bestEdges |> List.choose id
                  |> List.append edges
                  |> Some

let getDeadends terminals (edges: Graph) = 
   let isTerminal x = List.contains x terminals

   edges |> Seq.collect (fun (u, v, w) -> [u; v])
         |> Seq.filter (fun x -> not (isTerminal x))
         |> Seq.countBy id
         |> Seq.choose (fun (key, count) -> if count = 1 then Some key else None)
         |> List.ofSeq

let reduce (getDeadends: Graph -> Vertex list) (edges: Graph) =
   let notGoingToDeadEnd deadends (u, v, w) =
      not (List.contains u deadends || List.contains v deadends)

   let rec reduce edges =
      let deadends = getDeadends edges
      if deadends.Length > 0
      then reduce (edges |> List.filter (notGoingToDeadEnd deadends))
      else edges
   reduce edges

module Factory =
   let  createReduce terminals = reduce (getDeadends terminals)