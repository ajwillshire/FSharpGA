﻿namespace FSharpGA
open GATypes
open Blades

module MultiVectors =

        //Our multivectors will typically be of a single grade but mixed-grade multivectors are possible, e.g., for rotations
    type MultiVector = {blades:Blade list} with
        member this.Bases = this.blades |> List.map(fun x -> x.basis)
        member this.ElementsOfGrade(grade:int):Blade list = this.blades |> List.filter(fun x -> x.basis.Grade = grade)
        member this.MaxGrade:int = this.blades |> List.map(fun t -> t.basis.Grade) |> List.max
        member this.Space = this.blades |> List.map(fun t -> t.basis) 
                                        |> List.map(fun w -> w.DimList)
                                        |> fun dims -> [for d in dims do yield! d]
                                        |> Set.ofList
        
        //Addition
        static member (+) (mv1:MultiVector, mv2:MultiVector) = 
            let allBlades = [mv1.blades; mv2.blades]
            {blades = simplifyBladeList(allBlades)}

        //Geometric Product
        static member (*)  (mv1:MultiVector, mv2:MultiVector) = 
            let combinedSpace = {dimensions = Set.union mv1.Space mv2.Space} //Works out the combined space of terms

            //Get all combinations of products required - terms in mv1 * terms in mv2
            let allMults = mv1.blades |> List.map(fun x -> mv2.blades 
                                                           |> List.map(fun y -> multiplyBasisElements(x,y, combinedSpace)))

            {blades = simplifyBladeList(allMults)} //Returns a MultiVector with the simplified terms

        static member Pow (mv1:MultiVector, n:int) = 
            seq{1..(n-1)} |> Seq.fold(fun acc _ -> acc * mv1) mv1


    //For objects with more than one dimensions, we need to calculate the sign
    let createSimpleMultiVector(mag:float, dims:Dimension list):MultiVector = {blades = [createBlade(mag, dims)]}
   
    let createIdentity(space:Basis):MultiVector= {blades = [{magnitude=1.; basis = space}]}


        
