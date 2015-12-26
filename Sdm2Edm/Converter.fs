module Sdm2Edm.Converter

open Sdm
open Edm

let doNothing = ()

let checkTableSize = function
| RowsTable (_, [])
| ColumnsTable (_, []) -> doNothing
| RowsTable (_, cols) ->
    cols
    |> Seq.map (fun col -> (col.Heading.IsSome, col.Rows.Length))
    |> Seq.reduce (fun (preIsSome, preRows) (isSome, rows) ->
         if preIsSome <> isSome || preRows <> rows then
           raise (System.ArgumentException())
         else
           (isSome, rows))
    |> ignore
| ColumnsTable (_, rows) ->
    rows
    |> Seq.map (fun row -> (row.Heading.IsSome, row.Columns.Length))
    |> Seq.reduce (fun (preIsSome, preCols) (isSome, cols) ->
         if preIsSome <> isSome || preCols <> cols then
           raise (System.ArgumentException())
         else
           (isSome, cols))
    |> ignore

let adjustColumns (cells: Cell list) =
  let maxCol =
    cells
    |> Seq.map (fun cell -> cell.Column + cell.MergedColumns)
    |> Seq.max

  (cells, ([], []))
  ||> List.foldBack (fun cell (res, rows) ->
        if rows |> List.contains cell.Row then (cell::res, rows)
        else
          let cell = { cell with MergedColumns = cell.MergedColumns + (maxCol - cell.MergedColumns) - cell.Column }
          (cell::res, cell.Row::rows)
      )
  |> fst

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
    checkTableSize contents
    let converted = convertTableContents rule start contents
    let own = Cells.calcRange converted
    rule.ArroundTable(own, groups, converted)
and convertTableContents (rule: ConvertionRule) (start: ComponentRange) = function
| RowsTable (groups, contents) ->
    (start, contents)
    |> Seq.unfold (function
                   | start, x::xs ->
                       let converted, headerRange = convertRowsTableContents rule start groups x
                       let own = Cells.calcRange converted
                       let converted = rule.ArroundTableColumn(own, headerRange, groups, converted)
                       let next = Cells.calcRange converted |> ComponentRange.nextComponentStart
                       Some (converted, (next, xs))
                   | _ -> None)
    |> List.concat
| ColumnsTable (groups, contents) ->
    // TODO : 実装
    []
and convertRowsTableContents (rule: ConvertionRule) (start: ComponentRange) (groups: ColumnStyleGroup list) (contents: ColumnContents) =
  let headerCells =
    match contents.Heading with
    | Some header -> convertCell rule start header.StyleGroups header.Content
    | None -> []
  let headerRange, next =
    match headerCells with
    | [] -> dup start
    | _ ->
        let headerRange = Cells.calcRange headerCells
        (headerRange, (headerRange |> ComponentRange.nextComponentStart))
  let cells =
    (next, contents.Rows)
    |> Seq.unfold (function
                   | start, (groups, x)::rest ->
                       let converted = convertCell rule start groups x
                       let next = Cells.calcRange converted |> ComponentRange.nextComponentStart
                       Some (converted, (next, rest))
                   | _ -> None)
    |> List.concat
  let res =
    [ yield! headerCells; yield! cells ]
    |> adjustColumns
  (res, headerRange)
and convertColumnsTableContents (rule: ConvertionRule) (start: ComponentRange) (groups: RowStyleGroup list) (contents: RowContents) =
  // TODO : 実装
  []
and convertCell (rule: ConvertionRule) (start: ComponentRange) (groups: CellStyleGroup list) (cell: Component) =
  let converted = convertComponent rule start cell
  let own = Cells.calcRange converted
  rule.ArroundTableCell(own, groups, converted)

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
