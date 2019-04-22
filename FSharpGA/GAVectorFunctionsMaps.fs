namespace FSharpGA
open GATypes
open AuxiliaryFunctions

module GAVectorFunctionsMaps =

    let getKeySet(mapIn:Map<Dimension, _>):Set<Dimension> =
        mapIn |> Map.toSeq |> Seq.map(fst) |> Set.ofSeq

    let makeVector(vals:List<float>, space:Basis):Vector =
        List.zip (Set.toList space.dimensions) vals
            |> List.filter(fun(d,v) -> v <> 0.)
            |> Map.ofList

    let sortedDimensions(a:Vector, b:Vector):Set<Dimension> =
         Seq.append (Map.toSeq a) (Map.toSeq b)
          |> Seq.map fst
          |> Seq.sort
          |> Set.ofSeq

    //A blade is basically a list of the Dimensions that a Multivector contains
    let makeBlade(a:Vector, space:Basis)=
        space.DimList|> List.map(fun x -> (a |> Map.containsKey x))

    let countSwaps(a:Vector, b:Vector, space:Basis) =

        let aBlade = makeBlade(a, space) |> List.map(fun b -> if b then 1 else 0)
        let bBlade = makeBlade(b, space) |> List.map(fun b -> if b then 1 else 0)

        let bScan = List.scan (+) 0 bBlade
        let aTail = List.append aBlade [0]

        List.zip aTail bScan
              |> List.sumBy(fun x -> (fst x) * (snd x))

    let signSwaps(a:Vector, b:Vector, space:Basis) =
          match countSwaps(a,b, space) % 2 with
            | 0 -> 1
            | _ -> -1

    let xOrVectors(a:Vector, b:Vector) =

        let aMap = getKeySet a
        let bMap = getKeySet b

        let union = Set.union aMap bMap
        let intersect = Set.intersect aMap bMap
        Set.difference union intersect
 
    let pointwiseProduct(a:Vector, b:Vector, space:Basis)=
            
            let dimList = space.DimList
            let av = dimList |> List.map(fun p -> if a.ContainsKey(p) then a.[p] else 0.)
            let bv = dimList |> List.map(fun q -> if b.ContainsKey(q) then b.[q] else 0.)
            List.zip3 dimList av bv 
            |> List.map(fun (d, p,q) -> (d, p*q))

    let dotProduct(a:Vector, b:Vector, space:Basis):float=
        pointwiseProduct(a,b, space)
        |> Seq.map(snd)
        |> Seq.reduce(+)


    let makeBiVector(a:Vector, b:Vector, x:Dimension, y:Dimension) =

        let ax = if a.ContainsKey(x) then a.[x] else 0.
        let ay = if a.ContainsKey(y) then a.[y] else 0.
        let bx = if b.ContainsKey(x) then b.[x] else 0.
        let by = if b.ContainsKey(y) then b.[y] else 0.

        let coeff = ax * by - bx * ay
        {magnitude = coeff; basis = {dimensions = Set.ofList[x;y]}}


    let exteriorProduct(a:Vector, b:Vector, space:Basis)=

        let biVectors = combinations [] 2 (space.DimList)
        biVectors |> Seq.map(fun x -> makeBiVector(a, b, x.[0], x.[1]))

    let geometricProduct2D(a:Vector, b:Vector, space:Basis)=

        let grade2 = exteriorProduct(a,b,space) |> Seq.toList

        let grade1 = {magnitude = dotProduct(a,b,space)
                      basis = {dimensions = Set.empty}}

        Set.ofList(List.append [grade1] grade2)



        



