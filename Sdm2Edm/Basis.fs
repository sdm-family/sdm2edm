[<AutoOpen>]
module internal Sdm2Edm.Basis

let dup x = (x, x)
let mapFst f (x, y) = (f x, y)
let mapSnd f (x, y) = (x, f y)

[<AutoOpen>]
module Extensions =
  open System.Collections.Generic

  type List<'T> with
    member this.TryGet(i) =
      if i < this.Count then Some this.[i]
      else None
