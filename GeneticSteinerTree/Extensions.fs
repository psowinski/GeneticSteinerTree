namespace GeneticSteinerTree.Extensions

module List =
   /// Every iteration reduces head of list, and the rest is mapped by 'mapper'.
   /// First 'mapper' call get the full list.
   let collectTail (mapper: 'T list -> 'U seq) (list: 'T list): 'U list =
      let rec collect acc list =
         match list with
         | [] -> acc |> List.ofSeq
         | _::xs -> collect (Seq.append (mapper list) acc) xs
      collect Seq.empty list

   /// Returns first element selected by 'chooser' and list without this element.
   /// None if 'chooser' did not choose anything.
   let takeOut (chooser: 'T -> 'U option) (list: 'T list): ('U * 'T list) option =
      let rec take prev tail =
         match tail with
         | [] -> None
         | x::xs -> match chooser x with
                    | Some v -> Some (v, (prev |> List.rev) @ xs)
                    | None -> take (x::prev) xs
      take [] list

   /// Returns first element selected by 'chooser' otherwise None.
   let pickOpt (chooser: 'T -> 'U option) (list: 'T list): 'U option =
      let rec pick list =
         match list with
         | [] -> None
         | x::xs -> match chooser x with
                    | Some x -> Some x
                    | None -> pick xs
      pick list
