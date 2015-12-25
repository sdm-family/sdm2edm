[<AutoOpen>]
module TestUtils

open Sdm
open Sdm2Edm
open Edm

let noBorder = {
  Style = NoBorder
  Color = NoColor
}

let format =
  { RepresentationFormat = OneReprFormat { Color = None; Condition = None; Format = NumericFormat [ NFCLiteral "General" ] }
    Layout = { HorizontalLayout = HLStandard; VerticalLayout = VLCenter NoTextControl }
    Borders = { Top = noBorder; Right = noBorder; Bottom = noBorder; Left = noBorder; Diagonal = { Border = noBorder; TopLeftToBottomRight = false; BottomLeftToTopRight = false } }
    BackgroundColor = NoColor }

let emptyCell (row, col, height, width) =
  { Row = row; Column = col; MergedRows = height; MergedColumns = width; Format = format; Data = Other "" }

let text str =
  Text.fromTextSegment (TextSegment.fromString str)

let textCell { Address.Row = row; Column = col } txt = {
  Cell.Row = row
  Column = col
  MergedRows = 1
  MergedColumns = 26 - col
  Format = format
  Data = Other txt
}

let addr (row, col) = { Address.Row = row; Column = col }

let range (x, y) = (addr x, addr y)
