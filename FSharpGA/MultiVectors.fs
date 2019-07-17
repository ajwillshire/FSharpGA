namespace FSharpGA
open System
open GATypes
open Blades

module MultiVector =

        //Our multivectors will typically be of a single grade but mixed-grade multivectors are possible, e.g., for rotations
    type MultiVector = {blades:Blade list} with
        member this.Bases = this.blades |> List.map(fun x -> x.basis)
        member this.ElementsOfGrade(grade:int):Blade list = this.blades |> List.filter(fun x -> x.basis.Grade = grade)
        member this.MaxGrade:int = this.blades |> List.map(fun t -> t.basis.Grade) |> List.max
        member this.Space = this.blades |> List.map(fun t -> t.basis) 
                                        |> List.map(fun w -> w.DimList)
                                        |> fun dims -> [for d in dims do yield! d]
                                        |> Set.ofList
        
        member this.Components = this.blades |> List.map(fun b-> b.components)

        //Negation
        static member (~-) (mv1:MultiVector) = {blades = mv1.blades |> List.map(fun b -> {magnitude = -b.magnitude; basis = b.basis})}

        //Addition
        static member (+) (mv1:MultiVector, mv2:MultiVector) = 
            let allBlades = [mv1.blades; mv2.blades]
            {blades = simplifyBladeList(allBlades)}

        //Subtraction
        static member (-) (mv1:MultiVector, mv2:MultiVector) =
            mv1 + (-mv2)

        //Geometric Product
        static member (*)  (mv1:MultiVector, mv2:MultiVector) = 
            let combinedSpace = {dimensions = Set.union mv1.Space mv2.Space} //Works out the combined space of terms

            //Get all combinations of products required - terms in mv1 * terms in mv2
            let allMults = mv1.blades |> List.map(fun x -> mv2.blades 
                                                           |> List.map(fun y -> multiplyBasisElements(x,y, combinedSpace)))

            {blades = simplifyBladeList(allMults)} //Returns a MultiVector with the simplified terms

        static member Pow (mv1:MultiVector, n:int) = 
            seq{1..(n-1)} |> Seq.fold(fun acc _ -> acc * mv1) mv1


    //Variety of Constructors
    //let ofBlades(bladesIn:Blade list) = {blades = bladesIn} //Slightly unnecessary constructor objects with more than one dimensions, we need to calculate the sign
    
    let ofComponents(components:(float * Dimension list) list) =
        {blades = components |> List.map (fun (a,b) -> createBlade(a, b))} 
 
    let createSimpleMultiVector(mag:float, dims:Dimension list):MultiVector = {blades = [createBlade(mag, dims)]}

    let createVector(magList:float list, dimList:Dimension list):MultiVector = 
        {blades = List.zip magList dimList |> List.map (fun (a,b) -> createBlade(a, [b]))}


    //Identity methods
    let createIdentity(space:Basis):MultiVector= {blades = [{magnitude=1.; basis = space}]}

    let createIdentityInverse(space:Basis):MultiVector= 
        let n = float space.Grade
        let mag = (-1.)**(n*(n-1.)/2.)
        {blades = [{magnitude=mag; basis = space}]}

    //Rotation, reflection
    let makeRotor(degreesTheta:float, plane:Basis):MultiVector = // e^i.theta/2 = cos theta + i * sin theta
        let radThetaOverTwo = degreesTheta / 360. * Math.PI  //Divided by two for the operation
        //Create Multivector with scalar cos theta/2 and bivector with magnitude sin theta/2
        {blades = [createBlade(cos radThetaOverTwo, []); createBlade(sin radThetaOverTwo, plane.DimList)]} 

    let rotateMV(mv1:MultiVector, degreesTheta:float, plane:Basis):MultiVector = // e^-i.theta/2 * mv1 * e^i.theta/2
        let preR = makeRotor(-degreesTheta, plane)
        let postR = makeRotor(degreesTheta, plane)
        preR * mv1 * postR

    let reflectMV(mv1:MultiVector, dimension:Dimension):MultiVector = //Reflect in the plane perpendicular to the axis
        let axis = createSimpleMultiVector(1., [dimension])
        -axis * mv1 * axis
    