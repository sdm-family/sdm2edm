[<AutoOpen>]
module internal Sdm2Edm.Basis

let dup x = (x, x)
let mapFst f (x, y) = (f x, y)
let mapSnd f (x, y) = (x, f y)