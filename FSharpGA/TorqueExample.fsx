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
//Torque Example
let lever = {blades = [createBlade(0.76, [X])]}
let force = {blades = [createBlade(900., [Y])]}

let torque = lever * force //In GA, we don't need to create the "Z" Dimension


//The cross product is the dual of the exterior product
let space3D = {dimensions = Set.ofList [X;Y;Z]}
let Iinv = createIdentityInverse(space3D) //I is the "unit pseudoscalar", I^-1 = +/-I
let dual = torque * Iinv //Multiplying by the Inverse of the unit pseudoscalar gives the same answer as the Cross Product in 3D
                         //This works regardless of the size of the space - not limited to 3D
                         //Additionally, at no point have we had to define the "handedness" of the coordinate system

//In 3 dimensions, the regular calculations come up with the same answer (which is a good thing)
let leverV = vectorArray3D(lever)
let forceV = vectorArray3D(force)
let mag = crossProduct(leverV, forceV)


//****************************************************************************************


