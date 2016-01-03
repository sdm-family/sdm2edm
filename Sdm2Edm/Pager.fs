namespace Sdm2Edm

open Edm
open System.Collections.Generic

type Placeholder = PageNumber

[<AbstractClass>]
type Pager(info: PagerInfo) =
  let headerRange =
    Cells.calcRange info.PageHeader.Cells

  let mutable pageNo = 0
  let sheetNameTable = Dictionary<string, int>()

  do
    if info.MaxColumns < headerRange.End.Column then
      failwith "header width must be smaller than max columns of page."
    if info.MaxRows < headerRange.End.Row then
      failwith "header height must be smaller than max rows of page."

  member __.HeaderRange = headerRange

  member __.PageNo = pageNo
  member __.IncrPageNo() = pageNo <- pageNo + 1

  member __.CreateSheetName(sheetName) =
    match sheetNameTable.TryGetValue(sheetName) with
    | true, n ->
        sheetNameTable.[sheetName] <- n + 1
        sheetName + (sprintf "(%d)" (n + 1))
    | false, _ ->
        sheetNameTable.[sheetName] <- 1
        sheetName

  member this.RealizeHeader(row, col) =
    let template = info.PageHeader.Cells
    template
    |> Seq.map (fun cell ->
        match cell.Data with
        | Other (:? Placeholder) -> { cell with Data = Other pageNo }
        | _ -> cell)
    |> Seq.map (fun cell ->
        { cell with Row = cell.Row + row; Column = cell.Column + col })
    |> Seq.toList

  member this.Page(sheets) =
    pageNo <- 0
    sheetNameTable.Clear()
    sheets |> List.collect (fun sheet -> this.IncrPageNo(); this.SheetToPage(sheet))

  abstract SheetToPage: Sheet -> Sheet list

type BreakSheetPager(info: PagerInfo) =
  inherit Pager(info)

  let splitRow row cells =
    cells |> List.partition (fun cell -> cell.Row + cell.MergedRows < row + 1)

  override this.SheetToPage(sheet) =
    let headerRange = this.HeaderRange
    if List.isEmpty sheet.Cells then [{ sheet with Cells = this.RealizeHeader(0, 0) }]
    else
      let initSheetRange = Cells.calcRange sheet.Cells
      // このPagerでは、各シートの幅は必ずPageInfo.MaxColumns以下である必要がある
      if info.MaxColumns < initSheetRange.End.Column then
        failwith "sheet width must be smaller than max columns of page."

      sheet.Cells
      |> Seq.unfold (function
                     | [] -> None
                     | cells ->
                         // あふれたら新しいシートを作っていくため、ヘッダーの位置は(0, 0)固定でいい
                         let header = this.RealizeHeader(0, 0)
                         let cells =
                           [ yield! header
                             yield! cells
                                    |> List.map (fun cell ->
                                        { cell with
                                            Row = cell.Row + headerRange.End.Row + 1 }) ]
                         let oneSheetCells, rest = splitRow info.MaxRows cells
                         do
                           match rest with
                           | [] -> ()
                           | _ -> this.IncrPageNo()
                         let sheet = { Name = this.CreateSheetName(sheet.Name); Cells = oneSheetCells }
                         match rest with
                         | [] -> Some (sheet, [])
                         | rest ->
                             let min = rest.Head.Row
                             Some (sheet, rest |> List.map (fun cell -> { cell with Row = cell.Row - min })))
      |> Seq.toList

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Pager =
  let breakSheetPager info = BreakSheetPager(info) :> Pager