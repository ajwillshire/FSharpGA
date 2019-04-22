namespace FSharpGA
open GATypes
open AuxiliaryFunctions

module BladeFunctions = 

     //Creates integer representation of dimensions in a blade, e.g., [YZ] = [011] in [XYZ] space
    let makeBasisBlade(a:Basis, space:Basis):int[]=
        space.dimensions |> Set.toArray |> Array.map(fun x -> (if (a.dimensions |> Set.contains x ) then 1 else 0))

    //Counts the number of term swaps to order terms
    let countBladeSwaps(aBlade:int[], bBlade:int[]):int =
        let bScan = Array.scan (+) 0 bBlade //Scan(+) function increments with every non-zero element in int array
        let longA = Array.append aBlade [|0|] //Adds a zero on the end so that the arrays are the same length
        Array.zip longA bScan |> Array.sumBy(fun x -> (fst x) * (snd x)) //Sums the product terms

    //If number of swaps is even, term is positive, else negative
    let signBladeSwaps(aBlade:int[], bBlade:int[]):int =
            match countBladeSwaps(aBlade, bBlade) % 2 with
            | 0 -> 1
            | _ -> -1

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

    //Multiplies two blades together, grouping common terms, etc.
    let multiplyBasisElements(e:Blade, f:Blade, space:Basis):Blade =
        let eUnits = e.UnitBases(space)
        let fUnits = f.UnitBases(space)
        let sign = float(signBladeSwaps(eUnits, fUnits))
        let outputSpace = xOrBases(e.basis, f.basis) //What dimensions are we working in?
        let coeff = e.magnitude * f.magnitude * sign //Calculate the magnitude (scalar term) of the product
        {magnitude = coeff; basis = outputSpace} //Returns the blade representing the product


    
    let simplifyBladeList(combinedBlades:List<List<Blade>>) = 
        seq{for m in combinedBlades do yield! m} //Get a single set of all terms
                                |> Seq.groupBy(fun x-> x.basis) //Gather similar terms
                                |> Seq.map(fun (x,y) -> (x, (y |> Seq.sumBy(fun z -> z.magnitude)))) //Sum similar terms
                                |> Seq.filter(fun b -> snd b <> 0.) //Filter out zero terms
                                |> Seq.map(fun (p,q) -> {magnitude = q; basis = p}) //Convert to blades
                                |> Set.ofSeq