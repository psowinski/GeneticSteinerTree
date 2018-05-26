module GeneticSteinerTreeTests.Core.GenotypeTest
open GeneticSteinerTree.Core.Data
open GeneticSteinerTree.Core.Genotype
open Xunit

[<Fact>]
let ``Generate genotype from sequence of genes`` () = 
   let expected = Genotype [Active "a"; Active "b"]
   let actual = createGenotype (fun _ -> true) ["a"; "b"]
   Assert.Equal<Genotype>(expected, actual)

[<Fact>]
let ``Cross genotypes in specified point`` () = 
   let expected = [
                     Genotype [Active "a"; Inactive "b"; Inactive "c"];
                     Genotype [Inactive "a"; Active "b"; Active "c"]
                  ]
   let genotype1 = Genotype [Active "a"; Inactive "b"; Active "c"]
   let genotype2 = Genotype [Inactive "a"; Active "b"; Inactive "c"]
   let actual = crossGenotypes 2 genotype1 genotype2
   Assert.Equal<Genotype>(expected, actual)

[<Fact>]
let ``Mutate genotype in specified point`` () = 
   let expected = Genotype [Active "a"; Inactive "b"; Inactive "c"]
   let actual = mutateGenotype 2 (Genotype [Active "a"; Inactive "b"; Active "c"])
   Assert.Equal<Genotype>(expected, actual)