namespace FSharpGA
open System.Text

module GATypes = 

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

    //Essentially a MultiVector of single grade
    type Blade ={magnitude:float; basis:Basis} with
        member this.UnitBases(space:Basis) = space.dimensions 
                                                |> Set.toArray //Needs to be an Array as a Set will exclude repeated entries
                                                |> Array.map(fun x -> (if (this.basis.dimensions |> Set.contains x ) then 1 else 0))



