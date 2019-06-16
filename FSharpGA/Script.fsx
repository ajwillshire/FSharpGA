#load "GATypes.fs"
#load "AuxiliaryFunctions.fs"
#load "Blades.fs"
#load "GAin2D.fs"
#load "MultiVectors.fs"


open FSharpGA
open GATypes
open Blades
open MultiVectors



//****************************************************************************************
//Dimensions and sets

let space:Basis = {dimensions = Set.ofList [X;Y;Z]} //val space : Basis = {dimensions = set [X; Y; Z];}

//The advantage with using sets is that it reduces the errors that are possible in the initialisation
let space2 = {dimensions = Set.ofList [Y;Y;X;Z]} //val space2 : Basis = {dimensions = set [X; Y; Z];}

//****************************************************************************************



//let a = makeVector([0.; 1.; 1.], space)
//let b = makeVector([1.; 1.; 0.], space)

//let g = countSwaps(a,b,space)
//let h = xOrDimensions(a,b,space)
//let i = signSwaps(a,b,space)
//let h = GeometricProduct(a,b, space)
//let j = GeometricProduct(b,a, space);;


//combinations [] 2 (space.dimList)


//let t1 = makeVector([0.2;0.7;-0.2], space)
//let t2 = makeVector([-0.7;-0.2;0.5], space)

//let t1 = makeVector([5.;3.;0.], space)
//let t2 = makeVector([2.;0.;1.], space)

//let t3 = makeVector([1.;3.;-1.], space)
//let t4 = makeVector([2.;2.;-3.], space)


//let g1 = geometricProduct(t1,t2,space);;

//let g2 = geometricProduct(t3,t4,space);;

//let g3 = geometricProduct(t1,t4,space);;

//let m1 = MultiVectors.geometricProduct(g1, g2)
//let m2 = MultiVectors.geometricProduct(g1,g3)

//let mm1 = MultiVectors.geometricProduct(g3,m1)

let bv1 = MultiVectors.createSimpleMultiVector(10., [Y;X])
let j = MultiVectors.createSimpleMultiVector(1., [Z;X;Y])
let j2 = j ** 1
let k = j*bv1

let x1 =Blades.createVector(1., X)
let y1 =Blades.createVector(3., Y)
let z1 =Blades.createVector(0., Z)
let v1 = {blades = [x1;y1;z1]}

let x2 =createBlade(2., [X])
let y2 =createBlade(0., [Y])
let z2 =createBlade(1.,[Z])
let v2 = {blades = [x2;z2]}


let v3 = {blades = [x1]}
let v3sq = v3*v3

let e1e2 = {blades = [x1]} * {blades = [y1]}
let e2e1 = {blades = [y1]} * {blades = [x1]}


let mv1 = {blades = [createBlade(7., [X;Y])
                     createBlade(1., [Z])]}

let gp1 = v1 * v2;;
let gp2 = v1 * gp1;;

let gp3 = mv1 * bv1;;


