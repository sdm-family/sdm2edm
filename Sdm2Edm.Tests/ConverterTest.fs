module ConverterTest

open Persimmon
open UseTestNameByReflection

open Sdm
open Sdm2Edm
open Edm

let data (cell: Cell) =
  match cell.Data with
  | Other o -> o
  | _ -> failwithf "unsupported data constructor: %A" cell.Data

let segmentsToString (segments: TextSegment list) =
  segments |> List.map (fun seg -> seg.Value) |> String.concat ""

// このConvertionRuleの実装はテスト用なので参考にしないこと
// 本来はgroupsの値を見て変換方法を決定するようなコードを書く
let rule = { new ConvertionRule() with
               override __.Text(start, groups, text) = [ textCell start.Start (text.Segments |> segmentsToString) ]
               override __.ArroundHeading(start, groups, level, cells) =
                 let prefix = (String.replicate level "#") + " "
                 match cells with
                 | [] -> [ textCell start.Start prefix ]
                 | x::xs -> (textCell { Address.Row = x.Row; Column = x.Column } (prefix + (string (data x))))::xs
               override __.ArroundParagraph(start, groups, cells) = cells
               override __.ArroundListItem(start, groups, cells) =
                 let cells, _ = Cells.moveRight 1 cells
                 (textCell start.Start "*")::(cells |> List.map (fun cell -> { cell with MergedColumns = cell.MergedColumns - 1 }))
               override __.ArroundList(start, groups, cells) = cells 
               override __.ArroundTableCell(start, groups, cells) = cells
               override __.ArroundTableColumn(start, headerRange, groups, cells) = cells
               override __.ArroundTableRow(start, headerRange, groups, cells) = cells
               override __.ArroundTable(start, groups, cells) = cells
           }

let ``convertPageでAddressが計算できる`` = test {
  let page = {
    Page.Name = "for test"
    Components =
      [
        // 本来はStyleGroupを定義して、ConvertionRuleでStyleGroupによって変換方法を変えるようにするが、テストなので空([])でやる
        Heading ([], 1, text "heading1")
        Heading ([], 2, text "heading2")
        Paragraph ([], [ text "first line"; text "second line" ])
        Paragraph ([], [ text "other paragraph" ])
        List ([], [ Paragraph ([], [ text "list item 1" ])
                    Paragraph ([], [ text "list"; text "item"; text "1" ]) ])
      ]
  }
  let res = Converter.convertPage rule page
  let expected = {
    Sheet.Name = "for test"
    Cells =
      [
        textCell { Address.Row = 0; Column = 0 } "# heading1"
        textCell { Address.Row = 1; Column = 0 } "## heading2"
        textCell { Address.Row = 2; Column = 0 } "first line"
        textCell { Address.Row = 3; Column = 0 } "second line"
        textCell { Address.Row = 4; Column = 0 } "other paragraph"
        textCell { Address.Row = 5; Column = 0 } "*"
        textCell { Address.Row = 5; Column = 1 } "list item 1"
        textCell { Address.Row = 6; Column = 0 } "*"
        textCell { Address.Row = 6; Column = 1 } "list"
        textCell { Address.Row = 7; Column = 1 } "item"
        textCell { Address.Row = 8; Column = 1 } "1"
      ]
  }
  do! assertEquals expected res
}

let ``checkTableSizeでテーブルの行数もしくは列数が統一されていることがチェックできる`` =
  let test (table, thrownException) = test {
    if thrownException then
      let! e = trap { it (Converter.checkTableSize table) }
      do! assertEquals typeof<System.ArgumentException> (e.GetType())
    else
      Converter.checkTableSize table
      do! pass ()
  }
  parameterize {
    case (RowsTable ([], []), false)
    case (RowsTable ([], [{ Heading = None; Rows = [([], Paragraph([], []))] }
                          { Heading = None; Rows = [([], Paragraph([], []))] }]), false)
    case (RowsTable ([], [{ Heading = None; Rows = [] }
                          { Heading = None; Rows = [([], Paragraph([], []))] }]), true)
    case (ColumnsTable ([], []), false)
    case (ColumnsTable ([], [{ Heading = None; Columns = [([], Paragraph([], []))] }
                             { Heading = None; Columns = [([], Paragraph([], []))] }]), false)
    case (ColumnsTable ([], [{ Heading = None; Columns = [] }
                             { Heading = None; Columns = [([], Paragraph([], []))] }]), true)
    run test
  }

let ``adjustColumnsで列幅を統一できる`` =
  let test (cells, expected) = test {
    do! assertEquals expected (Converter.adjustColumns cells)
  }
  parameterize {
    case ([emptyCell (0, 0, 1, 1)], [emptyCell (0, 0, 1, 1)])
    case ([emptyCell (0, 0, 1, 1); emptyCell (1, 0, 10, 2)], [emptyCell (0, 0, 1, 2); emptyCell (1, 0, 10, 2)])
    case ([emptyCell (0, 0, 1, 3)
           emptyCell (1, 0, 1, 1); emptyCell (1, 1, 1, 1)
           emptyCell (2, 0, 1, 2); emptyCell (2, 2, 1, 3)
           emptyCell (3, 0, 1, 1)],
          [emptyCell (0, 0, 1, 5)
           emptyCell (1, 0, 1, 1); emptyCell (1, 1, 1, 4)
           emptyCell (2, 0, 1, 2); emptyCell (2, 2, 1, 3)
           emptyCell (3, 0, 1, 5)])
    run test
  }