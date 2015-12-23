module Sdm2Edm.Converter

open Sdm
open Edm

let convertComponent (rule: ConvertionRule) (c: Component) : Cell list =
  []

let convertPage (rule: ConvertionRule) (page: Page) : Sheet =
  let sheetName = page.Name
  { Sheet.Name = sheetName
    Cells = page.Components |> List.collect (convertComponent rule) }

let convert (rule: ConvertionRule) (pages: Page list) : Sheet list =
  pages |> List.map (convertPage rule)