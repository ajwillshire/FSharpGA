namespace FSharpGA
open GATypes
open AuxiliaryFunctions
open BladeFunctions

module MultiVectors =

        //Our multivectors will typically be of a single grade but mixed-grade multivectors are possible, e.g., for rotations
    type MultiVector = {blades:Set<Blade>} with
        member this.Bases = this.blades |> Set.map(fun x -> x.basis)
        member this.ElementsOfGrade(grade:int) = this.blades |> Set.filter(fun x -> x.basis.Grade = grade)
        member this.MaxGrade = this.blades |> Set.map(fun t -> t.basis.Grade) |> Set.maxElement
        member this.Space = this.blades |> Set.map(fun t -> t.basis) 
                                        |> Set.map(fun w -> w.DimList)
                                        |> fun dims -> [for d in dims do yield! d]
                                        |> Set.ofList
        
        static member (+) (mv1:MultiVector, mv2:MultiVector) = 
            let allBlades = [mv1.blades |> Set.toList; mv2.blades |> Set.toList]
            {blades = simplifyBladeList(allBlades)}

        //Geometric Product
        static member (*)  (mv1:MultiVector, mv2:MultiVector) = 
            let combinedSpace = {dimensions = Set.union mv1.Space mv2.Space} //Works out the combined space of terms

            //Get all combinations of products required - terms in mv1 * terms in mv2
            let allMults = mv1.blades |> Set.toList
                                      |> List.map(fun x -> mv2.blades 
                                                           |> Set.toList
                                                           |> List.map(fun y -> multiplyBasisElements(x,y, combinedSpace)))

            {blades = simplifyBladeList(allMults)} //Returns a MultiVector with the simplified terms

     //Simple helper functions to create common objects in the space
    let createScalar(mag:float) = {magnitude = mag; basis = {dimensions = Set.ofList[]}}
    let createVector(mag:float, dim:Dimension) = {magnitude = mag; basis = {dimensions = Set.ofList[dim]}}

    let createSimpleMultiVector(mag:float, dims:List<Dimension>) = {magnitude = mag * float (signDimSwaps(dims)); 
                                                                     basis = {dimensions = Set.ofList dims}}

    let createIdentity(space:Basis)= {magnitude = 1.; basis = {dimensions = space.dimensions}}

