namespace NLNagoya

open Edm

module Shapes =
  // TODO : EDMに移動
  let private toBold f = f |> Font.updateStyle BoldStyle

  // TODO : EDMに移動
  let private fontSize (f: FontInfo) =
    match f with
    | NoFontInfo -> failwith "oops!"
    | FontInfo f -> f.Size

  let private stext text =
    let segments = [ { RichTextSegment.Value = text; FontInfo = Font.noSpecific } ]
    let richText = RichText.create (segments, Font.noSpecific |> Font.updateName "メイリオ" |> toBold)
    ShapeText.create richText

  let private resize newSize (stext: ShapeText) =
    // TODO : EDMに移動
    let txt = { stext.Text with FontInfo = stext.Text.FontInfo |> Font.updateSize newSize }
    { stext with Text = txt }

  let private moveDown = function
  | RowColAndOffsetPixel (row, col) -> RowColAndOffsetPixel ({ row with Address = row.Address + 3 }, col)
  | _other -> failwith "oops!"

  let private moveRight = function
  | RowColAndOffsetPixel (row, col) -> RowColAndOffsetPixel (row, { col with Address = col.Address + 5 })
  | _other -> failwith "oops!"

  let private appN n f x =
    [1..n]
    |> List.fold (fun acc _ -> f acc) x

  let private arrow prefix no pos =
    let name = prefix + "arr" + string no
    let n = if no = 1 then 2 else (no * 2 + (no - 1))
    Shape.create name ShapeOfRightArrow  (pos |> moveDown |> appN n moveRight)
    |> Drawing.updateSize (Percent 50)

  let shape pos = function
  | "Overview" ->
      [ Shape.createWithText "ovv1" ShapeOfFlowChartAlternateProcess (stext "専用データ構造") pos
        arrow "ovv" 1 pos
        Shape.createWithText "ovv2" ShapeOfFlowChartAlternateProcess (stext "SDM") (pos |> appN 3 moveRight)
        arrow "ovv" 2 pos
        Shape.createWithText "ovv3" ShapeOfFlowChartAlternateProcess (stext "EDM") (pos |> appN 6 moveRight)
        arrow "ovv" 3 pos
        Shape.createWithText "ovv4" ShapeOfFlowChartAlternateProcess (stext "方眼紙") (pos |> appN 9 moveRight) ]
  | _ -> []
    