[<AutoOpen>]
module GeneticSteinerTree.Core.Data

type Vertex = string

type Gene =
| Active of Vertex
| Inactive of Vertex

type Genotype = | Genotype of Gene list
type Population = | Population of Genotype list

type Weight = float option
type RankedPopulation<'Genotype> = | RankedPopulation of ('Genotype * Weight) list

type RankingFunc = Gene seq -> Weight

type Graph = (Vertex * Vertex * float) list
type SteinerTree = Graph option
