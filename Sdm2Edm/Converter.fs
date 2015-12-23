module Sdm2Edm.Converter

open Sdm
open Edm

let convertComponent (r: Realizer) (c: Component) : Cell list =
  []

let convertPage (r: Realizer) (page: Page) : Sheet =
  let sheetName = page.Name
  { Sheet.Name = sheetName
    Cells = page.Components |> List.collect (convertComponent r) }

let convert (r: Realizer) (pages: Page list) : Sheet list =
  pages |> List.map (convertPage r)