namespace FSharpGA
open GATypes

module AuxiliaryFunctions =

    //Modified Tomas Petricek function
    //https://stackoverflow.com/questions/4495597/combinations-and-permutations-in-f
    let rec combinations acc size set = seq {
        match size, set with 
        | n, x::xs -> 
            if n > 0 then yield! combinations (x::acc) (n - 1) xs
            if n >= 0 then yield! combinations acc n xs 
        | 0, [] -> yield acc |> List.sort
        | _, [] -> () }


    //Modified Tomas Petricek function
    //https://stackoverflow.com/questions/279145/what-is-the-most-elegant-way-of-bubble-sorting-in-f
    let bubbleSort (arr:'a[]) = 
      let arr = arr |> Array.copy
      let mutable count:int = 0

      let swap i j = let tmp = arr.[i] in arr.[i] <- arr.[j]; arr.[j] <- tmp

      for i = arr.Length - 1 downto 0 do
        for j = 1 to i do
          if (arr.[j - 1] > arr.[j]) then 
            swap (j-1) j
            count <- (count+1)
      (arr, count)

    let makeAllBases(space:Basis)=

        let emptySet:Set<Dimension> = Set.empty //This represents the scalars in the dimensional space
        let grades = [1..(space.dimensions |> Set.count)] //The highest grade object is n-grade

        let basisSets = grades |> List.map(fun g -> (combinations [] g (space.DimList))) //Get the different grades of basis

        seq{for s in basisSets do yield! s} |> Seq.map(fun l -> {dimensions = l |> Set.ofList})
                                            |> Set.ofSeq
                                            |> Set.add {dimensions = emptySet}

