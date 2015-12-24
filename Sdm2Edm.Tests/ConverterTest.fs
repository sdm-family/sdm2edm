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

let segmentsToString (segments: TextSegment list) =
  segments |> List.map (fun seg -> seg.Value) |> String.concat ""

// このConvertionRuleの実装はテスト用なので参考にしないこと
// 本来はgroupsの値を見て変換方法を決定するようなコードを書く
let rule = { new ConvertionRule() with
               override __.Heading(address, groups, level, text) =
                 (address |> Address.updateRow 1, [ cell address (String.replicate level "#" + " " + (text.Segments |> segmentsToString)) ])
               override __.Paragraph(address, groups, lines) =
                 let nextAddress = address |> Address.updateRow (lines.Length + 1)
                 let cells =
                   lines
                   |> List.mapi (fun i line -> cell (address |> Address.updateRow i) (line.Segments |> segmentsToString))
                 (nextAddress, cells)
               override __.List(startAddress, endAddress, groups, cells) =
                 ({ Row = endAddress.Row + 1; Column = startAddress.Column }, cells)
               override __.ListItem(address, groups, itemCells) =
                 let res =
                   itemCells
                   |> List.mapi (fun i itemCell -> (itemCell, i))
                   |> List.collect (fun (itemCell, i) ->
                       [ if i = 0 then
                           yield cell { Address.Row = itemCell.Row; Address.Column = itemCell.Column; } "*"
                         yield { itemCell with Column = itemCell.Column + 1; MergedColumns = itemCell.MergedColumns - 1 } ])
                 (address |> Address.updateRow -1, res) }

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
        cell { Address.Row = 5; Column = 0 } "other paragraph"  // 上のParagraphとは別Paragraphなので、ConvertionRuleの実装によって空行が作られる
        cell { Address.Row = 7; Column = 0 } "*"                // ListとParagraphの間にも空行が作られる
        cell { Address.Row = 7; Column = 1 } "list item 1"      // リストのアイテムはカラムが一つずれ、元のカラムにはリストアイテムを表す「*」が入る
        cell { Address.Row = 8; Column = 0 } "*"
        cell { Address.Row = 8; Column = 1 } "list"
        cell { Address.Row = 9; Column = 1 } "item"             // リストのアイテムが複数行になっている場合、二行目以降は「*」は入らず、カラムがずれるだけ
        cell { Address.Row = 10; Column = 1 } "1"
      ]
  }
  do! assertEquals expected res
}