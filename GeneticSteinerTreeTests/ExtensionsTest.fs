module GeneticSteinerTreeTests.ExtensionsTest
open GeneticSteinerTree.Extensions
open Xunit

[<Fact>]
let ``Collect tail should process tail of collection`` () = 
   let expected = [1; 2; 2; 3; 3; 3]
   let actual = [1..3] |> List.collectTail (fun x -> x :> seq<int>)
                       |> List.sort
   Assert.Equal<int list>(expected, actual)

[<Fact>]
let ``Collect tail should work with empty list`` () = 
   let actual = [] |> List.collectTail (fun x -> x :> seq<int>)
   Assert.Equal<int list>([], actual)

[<Fact>]
let ``TakeOut should remove item from list`` () = 
   let expected = Some (6, [1; 3])
   let actual = [1..3] 
                |> List.takeOut (fun x -> if x = 2 then Some (x * 3) else None)
   Assert.Equal<(int * int list) option>(expected, actual)

[<Fact>]
let ``TakeOut should return None if nothing taken out`` () = 
   let actual = [1..3] |> List.takeOut (fun x -> None)
   Assert.Equal<(int * int list) option>(None, actual)

[<Fact>]
let ``TakeOut should work with empty list`` () = 
   let actual = [] |> List.takeOut (fun x -> Some x)
   Assert.Equal<(int * int list) option>(None, actual)

[<Fact>]
let ``TakeOut should remove only first found item`` () = 
   let expected = Some (2, [2])
   let actual = [2; 2] 
                |> List.takeOut (fun x -> if x = 2 then Some x else None)
   Assert.Equal<(int * int list) option>(expected, actual)

[<Fact>]
let ``PickOpt should return first found item`` () = 
   let expected = Some 5
   let actual = [1..5] 
                |> List.pickOpt (fun x -> if x = 5 then Some x else None)
   Assert.Equal<int option>(expected, actual)

[<Fact>]
let ``PickOpt should return None if nothing found`` () = 
   let actual = [1..5] |> List.pickOpt (fun x -> None)
   Assert.Equal<int option>(None, actual)

[<Fact>]
let ``PickOpt should work with empty list`` () = 
   let actual = [] |> List.pickOpt (fun x -> Some x)
   Assert.Equal<int option>(None, actual)
