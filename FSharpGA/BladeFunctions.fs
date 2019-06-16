namespace FSharpGA
open GATypes

module BladeFunctions = 

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
        let sorted = AuxiliaryFunctions.bubbleSort(dims |> List.toArray)
        match snd sorted % 2 with
               | 0 -> 1
               | _ -> -1

    //Cancels repeated terms, i.e. XX = 1
    let xOrBases(a:Basis, b:Basis):Basis =
        let union = Set.union a.dimensions b.dimensions
        let intersect = Set.intersect a.dimensions b.dimensions
        {dimensions = Set.difference union intersect} //XOR is the union of all terms less the intersection

