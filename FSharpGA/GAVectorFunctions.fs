namespace FSharpGA

module GAVectorFunctions =

    open GATypes

    let pointwiseProduct(a:Vector, b:Vector)=
            a |> Seq.append b
              |> Seq.groupBy(fun d -> d.Dimension)
              |> Seq.map (fun e -> fst e,snd e
                                           |> Seq.map(fun f -> f.Magnitude)
                                           |> Seq.reduce(*))

    let dotProduct(a:Vector, b:Vector):float=
        pointwiseProduct(a,b)
        |> Seq.map(snd)
        |> Seq.reduce(+)

    let makeVector(vals:List<float>, dims:List<Dimension>):List<Direction> =
        List.zip dims vals
            |> List.map(fun (dim, value) -> {Dimension = dim; Magnitude = value})
            |> List.filter(fun x -> x.Magnitude <> 0.)

    let sortedDimensions(a:Vector, b:Vector):List<Dimension> =
        a |> Seq.append b
          |> Seq.map (fun x -> x.Dimension)
          |> Seq.sort
          |> Seq.toList

    let makeBlade(a:Vector, dims:List<Dimension>)=
        dims |> List.map(fun x -> (a |> Seq.map(fun b -> b.Dimension))
                                        |> Seq.contains x)
                                        |> Seq.map(fun a -> if a then 1 else 0)
                                        |> Seq.toList

    let findDimensionValue(a:Vector, dimIn:Dimension):float =

        a |> List.filter(fun y -> y.Dimension = dimIn)
          |> List.tryExactlyOne 
          |> fun z -> match z with
                        | Some q -> q.Magnitude
                        | None -> 0.



    // let makeBivector(a:Vector, b:Vector, dims:List<Dimension>) =
        
    //     let partA = dims |> List.map(fun x -> findDimensionValue(a, x))
    //     let partB = dims |> List.map(fun x -> findDimensionValue(b, x))

    //     let value = partA.[0] * partB.[1] - partB.[0] * partA.[1]






    let countSwaps(a:Vector, b:Vector, dims:List<Dimension>) =

        let aBlade = makeBlade(a, dims)
        let bBlade = makeBlade(b, dims)

        let bScan = List.scan (+) 0 bBlade
        let aTail = List.append aBlade [0]

        List.zip aTail bScan
              |> List.sumBy(fun x -> (fst x) * (snd x))


    let signSwaps(a:Vector, b:Vector, dims:List<Dimension>) =
          match countSwaps(a,b, dims) % 2 with
            | 0 -> 1
            | _ -> -1

    let xOrDimensions(a:Vector, b:Vector):List<Dimension> =
          a |> List.append b
            |> List.groupBy(fun d -> d.Dimension)
            |> List.filter(fun e ->(snd(e) |> Seq.length = 1))
            |> List.map(fst)
            |> List.sort
            //|> Seq.toList

    
    
    // let outerProduct(a:Vector, b:Vector, dims:List<Dimension>) =
    
       


    // let GeometricProduct(a:Vector, b:Vector, dims:List<Dimension>) =
    //     {scalar = dotProduct(a,b) * (float (signSwaps(a, b, dims))) ;
    //     dimensions = xOrDimensions(a,b)}    
