#load "GATypes.fs"
#load "AuxiliaryFunctions.fs"
#load "Blades.fs"
#load "GAin2D.fs"
#load "MultiVectors.fs"
#load "VectorCalculations.fs"


open FSharpGA
open GATypes
open Blades
open MultiVector
open VectorCalculations

//****************************************************************************************
//Dimensions and sets

let space:Basis = {dimensions = Set.ofList [X;Y;Z]} //val space : Basis = {dimensions = set [X; Y; Z];}

//The advantage with using sets is that it reduces the errors that are possible in the initialisation
let space2 = {dimensions = Set.ofList [Y;Y;X;Z]} //val space2 : Basis = {dimensions = set [X; Y; Z];}

//****************************************************************************************



//let x1 =Blades.createBasisVector(1., X)
//let y1 =Blades.createBasisVector(3., Y)
//let z1 =Blades.createBasisVector(0., Z)
//let v1 = {blades = [x1;y1;z1]}
//let x2 =createBlade(2., [X])
//let y2 =createBlade(0., [Y])
//let z2 =createBlade(1.,[Z])
//let v2 = {blades = [x2;z2]}


//let v3 = {blades = [x1]}
//let v3sq = v3*v3



let e1 = createVector([1.], [X])
let e1sq = e1 **2

let v1 = createVector([1.;3.;0.], [X;Y;Z])
let v2 = createVector([2.;0.;1.], [X;Y;Z])

let gp1 = v1 * v2
let gp2 = v1 * gp1

let mv1 = {blades = [createBlade(7., [X;Y])
                     createBlade(1., [Z])]}

let bv1 = MultiVector.createSimpleMultiVector(10., [Y;X])
let j = MultiVector.createSimpleMultiVector(1., [X;Y;Z])
let j2 = j ** 2
let k = j*bv1


let gp3 = mv1 * bv1




//****************************************************************************************
//Reflection Example

//The vector will be reflected in the plane perpendicular to a1

let vec1 = {blades = [createBlade(3., [X]);createBlade(4., [Y]);createBlade(5., [Z])]}
let reflected = reflectMV(vec1, Y)


//****************************************************************************************



//****************************************************************************************
//Rotation Example

//let vec2 = MultiVector.ofBlades([createBlade(2., [X]); createBlade(3., [Y]);createBlade(4., [Z])])
//let vec2 = MultiVector.createVector([2.; 3.; 4.], [X;Y;Z])



let vec2 = MultiVector.ofComponents([(2., [X]); (3., [Y]); (4., [Z])])
let deg = 90.
let plane:Basis = {dimensions = Set.ofList [X;Y]}

let rotated = rotateMV(vec2, deg, plane)




rotated.Components;;

let vSq = vec2**2
//****************************************************************************************
