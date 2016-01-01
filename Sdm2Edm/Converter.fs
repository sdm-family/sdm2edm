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

let adjustColumns (cells: (int * int * Cell) list) =
  let maxCol =
    cells
    |> Seq.map (fun (_, _, cell) -> cell.Column + cell.MergedColumns)
    |> Seq.max

  (cells, ([], []))
  ||> List.foldBack (fun (r, c, cell) (res, rows) ->
        if rows |> List.contains cell.Row then ((r, c, cell)::res, rows)
        else
          let cell = { cell with MergedColumns = cell.MergedColumns + (maxCol - cell.MergedColumns) - cell.Column }
          ((r, c, cell)::res, cell.Row::rows)
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
    let contents =
      contents |> List.mapi (fun i c -> (i, c))
    (start, contents)
    |> Seq.unfold (function
                   | start, x::xs ->
                       let converted, headerRange = convertRowsTableContents rule start groups x
                       let convertedCells = converted |> List.map (fun (_, _, c) -> c)
                       let own = Cells.calcRange convertedCells
                       let convertedCells = rule.ArroundTableColumn(own, headerRange, groups, convertedCells)
                       let next = Cells.calcRange convertedCells |> ComponentRange.nextColumnStart
                       Some (converted, (next, xs))
                   | _ -> None)
    |> List.concat
    |> RowsTableConverter.Convert
| ColumnsTable (groups, contents) ->
    // TODO : 実装
    []
and convertRowsTableContents (rule: ConvertionRule) (start: ComponentRange) (groups: ColumnStyleGroup list) (colId, contents: ColumnContents) =
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
  let headerCells =
    headerCells |> List.map (fun c -> (0, colId, c))
  let offset = match contents.Heading with Some _ -> 1 | _ -> 0

  let cells =
    (next, contents.Rows |> List.mapi (fun i r -> (i + offset, r)))
    |> Seq.unfold (function
                   | start, (i, (groups, x))::rest ->
                       let converted = convertCell rule start groups x
                       let next = Cells.calcRange converted |> ComponentRange.nextComponentStart
                       Some (converted |> List.map (fun c -> (i, colId, c)), (next, rest))
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
