﻿module GeneticSteinerTree.Core.Genotype
open GeneticSteinerTree.Core.Graph

/// activator - returns true if vertex should be active gene otherwise false
let create activator genes =
   let createGene gene =
      if activator gene 
      then Active gene 
      else Inactive gene
   genes |> Seq.map createGene |> List.ofSeq |> Genotype

let cross position (Genotype firstGenes) (Genotype secondGenes) =
   [
      (List.take position firstGenes) @ (List.skip position secondGenes) |> Genotype
      (List.take position secondGenes) @ (List.skip position firstGenes) |> Genotype
   ]

let mutate position (Genotype genes) = 
   let revert = function
      | Active v -> Inactive v
      | Inactive v -> Active v

   let mutator idx gene =
      if idx = position then revert gene else gene

   genes |> List.mapi mutator |> Genotype

let length (Genotype genes) =
   List.length genes

let activeGenes (Genotype genes) = 
   genes |> List.choose (function | Active v -> Some v | _ -> None)
      