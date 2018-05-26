module GeneticSteinerTreeTests.Core.GenotypeShould
open GeneticSteinerTree.Core
open Data
open Genotype
open Xunit

[<Fact>]
let ``Generate genotype from sequence of genes`` () = 
   let expected = Genotype [Active "a"; Active "b"]
   let actual = Genotype.create (fun _ -> true) ["a"; "b"]
   Assert.Equal<Genotype>(expected, actual)

[<Fact>]
let ``Cross genotypes in specified point`` () = 
   let expected = [
                     Genotype [Active "a"; Inactive "b"; Inactive "c"];
                     Genotype [Inactive "a"; Active "b"; Active "c"]
                  ]
   let genotype1 = Genotype [Active "a"; Inactive "b"; Active "c"]
   let genotype2 = Genotype [Inactive "a"; Active "b"; Inactive "c"]
   let actual = Genotype.cross 2 genotype1 genotype2
   Assert.Equal<Genotype>(expected, actual)

[<Fact>]
let ``Mutate genotype in specified point`` () = 
   let expected = Genotype [Active "a"; Inactive "b"; Inactive "c"]
   let actual = Genotype.mutate 2 (Genotype [Active "a"; Inactive "b"; Active "c"])
   Assert.Equal<Genotype>(expected, actual)