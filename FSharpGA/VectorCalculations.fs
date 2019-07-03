namespace FSharpGA
open GATypes
open Blades
open MultiVector

module VectorCalculations =
    
    let space = {dimensions = Set.ofList [X;Y;Z]}
    type Vector = float[]

    let vectorArray3D(a:MultiVector):Vector =
            

            let dimList = space.DimList
            let comps = a.ElementsOfGrade(1) |> List.map(fun e ->  e.components) 
                                             |> List.map (fun (a,b) -> (b |> List.exactlyOne, a)) 
                                             |> Map.ofList

            dimList |> List.map(fun p -> if comps.ContainsKey(p) then comps.[p] else 0.) |> List.toArray

    let pointwiseProduct(v1:Vector, v2:Vector)=
            Array.zip v1 v2 
            |> Array.map(fun (p,q) -> p*q)

    let dotProduct(v1:Vector, v2:Vector):float = pointwiseProduct(v1, v2) |> Array.sum

    let vectorNorm(v1:Vector):float = v1 |> Array.map(fun a -> a**2.) |> Array.sum |> fun b -> b**0.5
        
    let vectorAngle(v1:Vector, v2:Vector):float =
        let dp = dotProduct(v1, v2)
        let n = vectorNorm(v1) * vectorNorm(v2)
        acos(dp / n)

    let crossProduct(a:Vector, b:Vector) =
        //let magnitude = vectorNorm(a)*vectorNorm(b) * sin(vectorAngle(a,b))
        let i = a.[1]*b.[2] - a.[2]*b.[1]
        let j = -(a.[0]*b.[2] - a.[2]*b.[0])
        let k = a.[0]*b.[1] - a.[1]*b.[0]
        let v:Vector =[|i;j;k|] 
        v

    let toMultiVector(v1:Vector) = MultiVector.createVector(v1 |> List.ofArray, space.DimList)

