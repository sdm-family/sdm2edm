namespace Sdm2Edm

open System.Collections.Generic

[<AbstractClass>]
type ConvertionRule () =
  let usedNamesCountTable = Dictionary<string, int>()

  abstract SheetName: string -> string

  default __.SheetName(name) =
    match usedNamesCountTable.TryGetValue(name) with
    | true, used ->
        usedNamesCountTable.[name] <- used + 1
        sprintf "%s(%d)" name (used + 1)
    | false, _ ->
        usedNamesCountTable.Add(name, 1)
        name