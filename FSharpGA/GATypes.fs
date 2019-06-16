namespace FSharpGA

module GATypes = 

    //We can combine whatever Dimensions that we want to suit the domain that we're working in
    type Dimension =
        | X
        | Y
        | Z
        | E1
        | E2
        | E3

    // Question here about whether the use of Set is correct - removes possibility of having both [X,Y] and [Y,X] as distinct
    // I think it's probably best given the way the other functions have been implemented
    type Basis = {dimensions:Set<Dimension>} with
        member this.Grade = this.dimensions |> Set.count
        member this.DimList = this.dimensions |> Set.toList






