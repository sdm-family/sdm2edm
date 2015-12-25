module Sdm2Edm.Converter

open Sdm
open Edm

let cellToRange (cell: Cell) =
  let startAddr = { Address.Row = cell.Row; Column = cell.Column }
  let endAttr = { Address.Row = cell.Row + cell.MergedRows - 1; Column = cell.Column + cell.MergedColumns - 1 }
  (startAddr, endAttr)

let calcRange (cells: Cell list) =
  let initRange = cellToRange cells.Head
  cells
  |> List.fold (fun ((startAddr: Address), (endAddr: Address)) cell ->
       let startAddr =
         { Address.Row = min startAddr.Row cell.Row
           Column = min startAddr.Column cell.Column }
       let endAddr =
         { Address.Row = max endAddr.Row (cell.Row + cell.MergedRows - 1)
           Column = max endAddr.Column (cell.Column + cell.MergedColumns - 1) }
       (startAddr, endAddr)
     ) initRange
  |> fun (s, e) -> { Start = s; End = e }

let dup x = (x, x)
let mapSnd f (x, y) = (x, f y)

let moveDown rows (cells: Cell list) =
  cells |> List.map (fun cell -> { cell with Row = cell.Row + rows }) |> dup |> mapSnd calcRange

let moveRight cols (cells: Cell list) =
  cells |> List.map (fun cell -> { cell with Column = cell.Column + cols }) |> dup |> mapSnd calcRange

let rec convertComponent (rule: ConvertionRule) (start: ComponentRange) = function
| Heading (groups, level, text) ->
    let cells = rule.Text(start, groups, text)
    rule.ArroundHeading(calcRange cells, groups, level, cells)
| Paragraph (groups, lines) ->
    let cells =
      (start, lines)
      |> Seq.unfold (function
                     | start, x::xs ->
                         let converted = rule.Text(start, groups, x)
                         let next = calcRange converted |> ComponentRange.nextComponentStart
                         Some (converted, (next, xs))
                     | _ -> None)
      |> List.concat
    let range = calcRange cells
    rule.ArroundParagraph(range, groups, cells)
| List (groups, items) ->
    let cells =
      (start, items)
      |> Seq.unfold (function
                     | start, x::xs ->
                         let converted = convertComponent rule start x
                         let own = calcRange converted
                         let converted = rule.ArroundListItem(own, groups, converted)
                         let next = calcRange converted |> ComponentRange.nextComponentStart
                         Some (converted, (next, xs))
                      | _ -> None)
      |> List.concat
    let range = calcRange cells
    rule.ArroundList(range, groups, cells)
| Table (groups, contents) ->
    []

let convertPage (rule: ConvertionRule) (page: Page) : Sheet =
  let cells =
    (ComponentRange.A1, page.Components)
    |> Seq.unfold (function
                   | start, x::xs ->
                       let converted = convertComponent rule start x
                       let range = calcRange converted
                       let next = ComponentRange.nextComponentStart range
                       Some (converted, (next, xs))
                   | _ -> None)
    |> List.concat

  { Sheet.Name = page.Name; Cells = cells }

let convert (rule: ConvertionRule) (pages: Page list) : Sheet list =
  pages |> List.map (convertPage rule)
