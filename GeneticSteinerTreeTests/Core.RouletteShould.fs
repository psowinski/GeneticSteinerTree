module GeneticSteinerTreeTests.Core.RouletteShould
open GeneticSteinerTree.Core
open Data
open Roulette
open Xunit
open System

let sortRoulette (Roulette (roulette, range)) =
   Roulette ((roulette |> List.sortBy (fun (x, _, _) -> x)), range)

[<Fact>]
let ``Create roulette according to weight`` () = 
   let expected = Roulette ([(1, 0, 20); (2, 20, 100)], 100)
   let actual = Roulette.create 100 (RankedPopulation [(1, Some 2.0); (2, Some 8.0)])
                |> sortRoulette
   Assert.Equal<Roulette<int>>(expected, actual)

[<Fact>]
let ``Roulette should not contain weight`` () = 
   let expected = Roulette ([(1, 0, 100)], 100)
   let actual = Roulette.create 100 (RankedPopulation [(1, Some 2.0); (2, None)])
                |> sortRoulette
   Assert.Equal<Roulette<int>>(expected, actual)

[<Fact>]
let ``Roulette should round last element to maximum range`` () = 
   let expected = Roulette ([(1, 0, 33); (2, 33, 66); (3, 66, 100)], 100)
   let actual = Roulette.create 100 (RankedPopulation [(1, Some 3.0); (2, Some 3.0); (3, Some 3.0)])
                |> sortRoulette
   Assert.Equal<Roulette<int>>(expected, actual)

[<Fact>]
let ``Roulette should work with empty`` () = 
   let actual = Roulette.create 100 (RankedPopulation [])
   Assert.Equal<Roulette<int>>(Roulette ([], 100), actual)

[<Fact>]
let ``Roulette should be split to all invalid if none is valid`` () = 
   let expected = Roulette ([(1, 0, 50); (2, 50, 100)], 100)
   let actual = Roulette.create 100 (RankedPopulation [(1, None); (2, None)])
                |> sortRoulette
   Assert.Equal<Roulette<int>>(expected, actual)

[<Theory>]
[<InlineData (1, 49)>]
[<InlineData (2, 50)>]
let ``Select should choose items by provided function`` (element, rndValue) = 
   let expected = List.init 2 (fun _ -> element)
   let roulette = Roulette ([(1, 0, 50); (2, 50, 100)], 100)
   let actual = Roulette.select (fun _ -> rndValue) 2 roulette
   Assert.Equal<int list>(expected, actual)

[<Theory>]
[<InlineData (1, 0)>]
[<InlineData (2, 50)>]
[<InlineData (3, 66)>]
let ``Running roulette should select elements acording to weight`` (element, rndValue) = 
   let expected = List.init 3 (fun _ -> element)
   let population = RankedPopulation [(1, Some 33.0); (2, Some 33.0); (3, Some 33.0)]
   let runRoulette = Roulette.Factory.createRun (fun _ -> rndValue) 100
   let actual = runRoulette population 3
   Assert.Equal<int list>(expected, actual)
