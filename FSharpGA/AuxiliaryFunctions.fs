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

//****************************************************************************************
//Functions for handling Dimensions, etc.

    //Creates integer representation of dimensions in a blade, e.g., [YZ] = [011] in [XYZ] space
    let convertBoolBladeToInt(a:bool[]):int[] = a |> Array.map (fun x -> match x with
                                                                           | true -> 1
                                                                           | false -> 0)

    //Counts the number of term swaps to order terms
    let countBladeSwaps(aBlade:bool[], bBlade:bool[]):int =

       //Convert boolean arrays to integer to allow scans and sumBys, etc.
       let aBladeInt = convertBoolBladeToInt(aBlade)
       let bBladeInt = convertBoolBladeToInt(bBlade)

       let bScan = Array.scan (+) 0 bBladeInt //Scan(+) function increments with every non-zero element in int array
       let longA = Array.append aBladeInt [|0|] //Adds a zero on the end so that the arrays are the same length
       Array.zip longA bScan |> Array.sumBy(fun x -> (fst x) * (snd x)) //Sums the product terms

    //If number of swaps is even, term is positive, else negative
    let signBladeSwaps(aBlade:bool[], bBlade:bool[]):int =
           match countBladeSwaps(aBlade, bBlade) % 2 with
           | 0 -> 1
           | _ -> -1

    //This is used in the creation of multivectors to get the dimensions the right way round, e.g., YX = -XY
    let signDimSwaps(dims:List<Dimension>) =
       let sorted = bubbleSort(dims |> List.toArray)
       match snd sorted % 2 with
              | 0 -> 1
              | _ -> -1

    //Cancels repeated terms, i.e. XX = 1
    let xOrBases(a:Basis, b:Basis):Basis =
       let union = Set.union a.dimensions b.dimensions
       let intersect = Set.intersect a.dimensions b.dimensions
       {dimensions = Set.difference union intersect} //XOR is the union of all terms less the intersection


