#load "GATypes.fs"
#load "AuxiliaryFunctions.fs"
#load "BladeFunctions.fs"
#load "GAin2D.fs"
#load "MultiVectors.fs"


open FSharpGA
open GATypes
open MultiVectors

let space:Basis = {dimensions = Set.ofList [X;Y;Z]}
 //let space:List<Dimension> = [E1;E2;E3]


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

let bv2 = MultiVectors.createSimpleMultiVector(10., [Y;X])


let x1 ={magnitude = 5.; basis = {dimensions = Set.ofList[X]}}
let y1 ={magnitude = 3.; basis = {dimensions = Set.ofList[Y]}}
//let z1 ={magnitude = 0.; basis = {dimensions = Set.ofList[Z]}}
let v1 = {blades = Set.ofList[x1;y1]}

let x2 ={magnitude = 2.; basis = {dimensions = Set.ofList[X]}}
//let y2 ={magnitude = 0.; basis = {dimensions = Set.ofList[Y]}}
let z2 ={magnitude = 1.; basis = {dimensions = Set.ofList[Z]}}
let v2 = {blades = Set.ofList[x2;z2]}

let bv1 = {blades = Set.ofList [{magnitude = 7.; basis = {dimensions = Set.ofList[X;Y]}}
                                {magnitude = 1.; basis = {dimensions = Set.ofList[Z]}}]}



let gp1 = v1 * v2;;
let gp2 = v1 * gp1;;

let gp3 = v2 * bv1;;