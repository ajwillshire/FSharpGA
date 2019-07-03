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

let bv1 = MultiVector.createSimpleMultiVector(10., [Y;X])
let j = MultiVector.createSimpleMultiVector(-1., [X;Y;Z])
let j2 = j ** 1
let k = j*bv1

let x1 =Blades.createBasisVector(1., X)
let y1 =Blades.createBasisVector(3., Y)
let z1 =Blades.createBasisVector(0., Z)
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




//****************************************************************************************
//Reflection Example

//The vector will be reflected in the plane perpendicular to a1
let a1 = createSimpleMultiVector(1., [Y])
let vec1 = {blades = [createBlade(3., [Y]);createBlade(4., [Z])]}

let reflected = -a1 * vec1 * a1

//****************************************************************************************



//****************************************************************************************
//Rotation Example

//The vector will be rotated
let a2 = createSimpleMultiVector(4., [X])
let b2 = createSimpleMultiVector(1., [Y])
let ab2 = a2 * b2
let ab3 = b2*a2
let aa = a2*a2

//let vec2 = MultiVector.ofBlades([createBlade(2., [X]); createBlade(3., [Y]);createBlade(4., [Z])])
//let vec2 = MultiVector.createVector([2.; 3.; 4.], [X;Y;Z])
let vec2 = MultiVector.ofComponents([(2., [X]); (3., [Y]); (4., [Z])])
let vSq = vec2**2

let vec3 = MultiVector.ofBlades([createBlade(1., [X;Z])])

//let jj = vec3 * vec3

let rotated = vec3 * vec2 * vec3 //ab2 * vec2 * ab2

vec2.Components;;
rotated.Components;;
vec3.Components;;

//****************************************************************************************



let a = 2
let p = 4

seq{1..(p-1)} |> Seq.fold(fun acc _ -> acc * a) a

seq{1..p} |> Seq.fold(fun acc _ -> acc * a) 1