module ConverterTest

open Persimmon
open UseTestNameByReflection

open Sdm
open Sdm2Edm
open Edm

let noBorder = {
  Style = NoBorder
  Color = NoColor
}

let cell { Address.Row = row; Column = col } txt = {
  Cell.Row = row
  Column = col
  MergedRows = 1
  MergedColumns = 26 - col
  Format = { RepresentationFormat = OneReprFormat { Color = None; Condition = None; Format = NumericFormat [ NFCLiteral "General" ] }
             Layout = { HorizontalLayout = HLStandard; VerticalLayout = VLCenter NoTextControl }
             Borders = { Top = noBorder; Right = noBorder; Bottom = noBorder; Left = noBorder; Diagonal = { Border = noBorder; TopLeftToBottomRight = false; BottomLeftToTopRight = false } }
             BackgroundColor = NoColor }
  Data = Other txt
}

let data (cell: Cell) =
  match cell.Data with
  | Other o -> o
  | _ -> failwithf "unsupported data constructor: %A" cell.Data

let segmentsToString (segments: TextSegment list) =
  segments |> List.map (fun seg -> seg.Value) |> String.concat ""

// このConvertionRuleの実装はテスト用なので参考にしないこと
// 本来はgroupsの値を見て変換方法を決定するようなコードを書く
let rule = { new ConvertionRule() with
               override __.Text(start, groups, text) = [ cell start.Start (text.Segments |> segmentsToString) ]
               override __.ArroundHeading(start, groups, level, cells) =
                 let prefix = (String.replicate level "#") + " "
                 match cells with
                 | [] -> [ cell start.Start prefix ]
                 | x::xs -> (cell { Address.Row = x.Row; Column = x.Column } (prefix + (string (data x))))::xs
               override __.ArroundParagraph(start, groups, cells) = cells
               override __.ArroundListItem(start, groups, cells) =
                 let cells, _ = Converter.moveRight 1 cells
                 (cell start.Start "*")::(cells |> List.map (fun cell -> { cell with MergedColumns = cell.MergedColumns - 1 }))
               override __.ArroundList(start, groups, cells) =
                 cells 
           }

let text str =
  Text.fromTextSegment (TextSegment.fromString str)

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
        cell { Address.Row = 0; Column = 0 } "# heading1"
        cell { Address.Row = 1; Column = 0 } "## heading2"
        cell { Address.Row = 2; Column = 0 } "first line"
        cell { Address.Row = 3; Column = 0 } "second line"
        cell { Address.Row = 4; Column = 0 } "other paragraph"
        cell { Address.Row = 5; Column = 0 } "*"
        cell { Address.Row = 5; Column = 1 } "list item 1"
        cell { Address.Row = 6; Column = 0 } "*"
        cell { Address.Row = 6; Column = 1 } "list"
        cell { Address.Row = 7; Column = 1 } "item"
        cell { Address.Row = 8; Column = 1 } "1"
      ]
  }
  do! assertEquals expected res
}
