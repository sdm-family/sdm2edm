module Sdm2Edm.Converter

open Sdm
open Edm

let rec convertComponent (rule: ConvertionRule) (start: ComponentRange) = function
| Heading (groups, level, text) ->
    let cells = rule.Text(start, groups, text)
    rule.ArroundHeading(Cells.calcRange cells, groups, level, cells)
| Paragraph (groups, lines) ->
    let cells =
      (start, lines)
      |> Seq.unfold (function
                     | start, x::xs ->
                         let converted = rule.Text(start, groups, x)
                         let next = Cells.calcRange converted |> ComponentRange.nextComponentStart
                         Some (converted, (next, xs))
                     | _ -> None)
      |> List.concat
    let range = Cells.calcRange cells
    rule.ArroundParagraph(range, groups, cells)
| List (groups, items) ->
    let cells =
      (start, items)
      |> Seq.unfold (function
                     | start, x::xs ->
                         let converted = convertComponent rule start x
                         let own = Cells.calcRange converted
                         let converted = rule.ArroundListItem(own, groups, converted)
                         let next = Cells.calcRange converted |> ComponentRange.nextComponentStart
                         Some (converted, (next, xs))
                      | _ -> None)
      |> List.concat
    let range = Cells.calcRange cells
    rule.ArroundList(range, groups, cells)
| Table (groups, contents) ->
    []

let convertPage (rule: ConvertionRule) (page: Page) : Sheet =
  let cells =
    (ComponentRange.A1, page.Components)
    |> Seq.unfold (function
                   | start, x::xs ->
                       let converted = convertComponent rule start x
                       let range = Cells.calcRange converted
                       let next = ComponentRange.nextComponentStart range
                       Some (converted, (next, xs))
                   | _ -> None)
    |> List.concat

  { Sheet.Name = page.Name; Cells = cells }

let convert (rule: ConvertionRule) (pages: Page list) : Sheet list =
  pages |> List.map (convertPage rule)
