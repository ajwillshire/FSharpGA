namespace FSharpGA
open GATypes
open AuxiliaryFunctions

module Blades =

    //A Blade is a MultiVector of single grade (e.g., "X" or "YZ" or "XYZ")
    type Blade = {magnitude:float; basis:Basis} with

        //Return a Boolean array to show True/False whether a dimension is present
        member this.UnitBases(space:Basis):bool[] = space.dimensions 
                                                    |> Set.toArray //Needs to be an Array as a Set will 
                                                                    //exclude repeated entries (not helpful with trues/falses)
                                                    |> Array.map(fun x -> this.basis.dimensions |> Set.contains x)

    //This constructor makes sure the magnitude is of the correct sign given the order of dimensions given                                                    
    let createBlade(mag:float, dims:Dimension list) = {magnitude = mag * float (signDimSwaps(dims)); basis = {dimensions = Set.ofList dims}}

    //Helper functions to create common objects in the space
    let createScalar(mag:float):Blade = createBlade(mag, []) //Zero dimensions
    let createVector(mag:float, dim:Dimension):Blade = createBlade(mag, [dim]) //One dimension


    //Multiplies two blades together, grouping common terms, etc.
    let multiplyBasisElements(e:Blade, f:Blade, space:Basis):Blade =
        let eUnits = e.UnitBases(space)
        let fUnits = f.UnitBases(space)
        let sign = float(signBladeSwaps(eUnits, fUnits))
        let outputSpace = xOrBases(e.basis, f.basis) //What dimensions are we working in?
        let coeff = e.magnitude * f.magnitude * sign //Calculate the magnitude (scalar term) of the product
        {magnitude =coeff; basis = outputSpace} //Returns the blade representing the product
        
    let simplifyBladeList(combinedBlades:List<List<Blade>>) = 
        seq{for m in combinedBlades do yield! m} //Get a single set of all terms
                                |> Seq.groupBy(fun x-> x.basis) //Gather similar terms
                                |> Seq.map(fun (x,y) -> (x, (y |> Seq.sumBy(fun z -> z.magnitude)))) //Sum similar terms
                                |> Seq.filter(fun b -> snd b <> 0.) //Filter out zero terms
                                |> Seq.map(fun (p,q) -> {magnitude = q; basis = p}) //Convert to blades
                                |> List.ofSeq





////A Blade is a MultiVector of single grade (e.g., "X" or "YZ" or "XYZ")
//type Blade(magnitude:float, basis:Basis) =

//    let mutable _magnitude = magnitude
//    let mutable _basis = basis

//    member this.magnitude with get() = _magnitude and set(value) = _magnitude <-value
//    member this.basis with get() = _basis and set(value) = _basis <-value

//    new(mag:float, dims:Dimension list) = Blade(mag * float (signDimSwaps(dims)), basis = {dimensions = Set.ofList dims})

//    //Return a Boolean array to show True/False whether a dimension is present
//    member this.UnitBases(space:Basis):bool[] = space.dimensions 
//                                                |> Set.toArray //Needs to be an Array as a Set will 
//                                                               //exclude repeated entries (not helpful with trues/falses)
//                                                |> Array.map(fun x -> this.basis.dimensions |> Set.contains x)