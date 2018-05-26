module GeneticSteinerTreeTests.Core.Population.RouletteShould
open GeneticSteinerTree.Core.Data
open GeneticSteinerTree.Core.Population.Roulette
open Xunit
open System

[<Fact>]
let ``Create roulette according to weight`` () = 
   let expected = [(1, 0, 20); (2, 20, 100)]
   let actual = createRoulette 100 (RankedPopulation [(1, Some 2.0); (2, Some 8.0)])
                |> List.sortBy (fun (x, _, _) -> x)
   Assert.Equal<(int * int * int) list>(expected, actual)

[<Fact>]
let ``Roulette should not contain weight`` () = 
   let expected = [(1, 0, 100);]
   let actual = createRoulette 100 (RankedPopulation [(1, Some 2.0); (2, None)])
                |> List.sortBy (fun (x, _, _) -> x)
   Assert.Equal<(int * int * int) list>(expected, actual)

[<Fact>]
let ``Roulette should round last element to maximum range`` () = 
   let expected = [(1, 0, 33); (2, 33, 66); (3, 66, 100)]
   let actual = createRoulette 100 (RankedPopulation [(1, Some 3.0); (2, Some 3.0); (3, Some 3.0)])
                |> List.sortBy (fun (x, _, _) -> x)
   Assert.Equal<(int * int * int) list>(expected, actual)

[<Fact>]
let ``Roulette should work with empty`` () = 
   let actual = createRoulette 100 (RankedPopulation [])
   Assert.Equal<(int * int * int) list>([], actual)

[<Fact>]
let ``Roulette should be split to all invalid if none is valid`` () = 
   let expected = [(1, 0, 50); (2, 50, 100);]
   let actual = createRoulette 100 (RankedPopulation [(1, None); (2, None)])
                |> List.sortBy (fun (x, _, _) -> x)
   Assert.Equal<(int * int * int) list>(expected, actual)

[<Theory>]
[<InlineData (1, 0)>]
[<InlineData (2, 50)>]
[<InlineData (3, 66)>]
let ``Running roulette should select elements acording to weight`` (element, rndValue) = 
   let expected = List.init 3 (fun _ -> element)
   let population = RankedPopulation [(1, Some 33.0); (2, Some 33.0); (3, Some 33.0)]
   let actual = rouletteSelection 100 (fun _ -> rndValue) population
   Assert.Equal<int list>(expected, actual)

[<Fact>]
let ``Running roulette should not work on precision less then 100`` () = 
   Assert.Throws<Exception>(fun () -> rouletteSelection 1 (fun _ -> 0) (RankedPopulation []) |> ignore)
