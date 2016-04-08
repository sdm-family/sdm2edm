namespace NLNagoya

open Sdm2Edm
open Edm

open System
open System.Text

type ConvertionRule(width: int, height: int) =
  inherit Sdm2Edm.ConvertionRule()

  let heightUnit = 22<pixel>
  let widthUnit = 20<pixel>

  let imageRow = System.Collections.Generic.Dictionary<_, _>()

  let mutable i = 0

  // TODO : Sdmに移動
  let size font =
    match font with
    | FontInfo font -> match font.Size with FontSize size -> size | NoFontSize -> failwith "oops!"
    | NoFontInfo -> failwith "oops!"

  // TODO : Sdm2Edmに移動
  let toEdmSegment (seg: Sdm.TextSegment) =
    { RichTextSegment.Value = seg.Value; FontInfo = Font.noSpecific }

  let fit (boxWidth, boxHeight) data =
    // boxHeightを基準にフォントサイズを決定
    let fontSize = 0.364 * (float boxHeight) |> int |> float
    // TODO : はみ出るようならboxWidthを基準にフォントサイズを決定
    data
    |> Data.editRichText (fun txt -> { txt with FontInfo = txt.FontInfo |> Font.updateSize fontSize |> Font.updateName "メイリオ" })

  let updateTitlePageFont data =
    data
    |> Data.editRichText (fun txt -> { txt with FontInfo = txt.FontInfo |> Font.updateName "Yu Gothic" })

  let updateHeadingFont data =
    data
    |> Data.editRichText (fun txt -> { txt with FontInfo = txt.FontInfo |> Font.updateStyle BoldStyle })

  let highlightUpper data =
    let splitAndToRed (seg: RichTextSegment) =
      let str = seg.Value
      let mutable isUpper = Char.IsUpper(str.[0])
      let buf = StringBuilder()
      [ for ch in str do
          if isUpper then
            if Char.IsUpper(ch) then
              buf.Append(ch) |> ignore
            else
              yield { seg with Value = buf.ToString(); FontInfo = seg.FontInfo |> Font.updateColor (Rgb(255, 0, 0)) }
              buf.Clear().Append(ch) |> ignore
              isUpper <- false
          else
            if not (Char.IsUpper(ch)) then
              buf.Append(ch) |> ignore
            else
              yield { seg with Value = buf.ToString(); FontInfo = seg.FontInfo |> Font.updateColor RgbColor.black }
              buf.Clear().Append(ch) |> ignore
              isUpper <- true
        yield { seg with Value = buf.ToString()
                         FontInfo =
                           if isUpper then seg.FontInfo |> Font.updateColor (Rgb(255, 0, 0))
                           else seg.FontInfo |> Font.updateColor RgbColor.black } ]
    data
    |> Data.editRichText (fun txt -> { txt with Segments = txt.Segments |> List.collect splitAndToRed })

  override __.Text(start, _groups, text) =
    [ Cell.richText (start.Start.Row, start.Start.Column) (1, 1) (RichText.createWithoutFontInfo (text |> Sdm.Text.map toEdmSegment)) ]
  override __.ArroundHeading(_start, groups, _level, cells) =
    match groups with
    | Sdm.Patterns.Contains Styles.mainTitle ->
        let vmiddle = height / 2
        let hmiddle = width / 2
        let titleHeight = int (float height * 0.15)
        let titleWidth = int (float width * 0.75)
        let titleHeightPx = titleHeight * heightUnit
        let titleWidthPx = titleWidth * widthUnit
        cells |> List.map (fun cell ->
                             { cell with
                                 Row = vmiddle - (titleHeight / 2); MergedRows = titleHeight
                                 Column = hmiddle - (titleWidth / 2); MergedColumns = titleWidth
                                 Data = fit (titleWidthPx, titleHeightPx) cell.Data |> updateTitlePageFont |> highlightUpper })
    | Sdm.Patterns.Contains Styles.subTitle ->
        let subTitleHeight = int (float height * 0.07)
        let subTitleWidth = int (float width * 0.75)
        let subTitleHeightPx = subTitleHeight * heightUnit
        let subTitleWidthPx = subTitleWidth * widthUnit
        cells |> List.map (fun cell ->
                             { cell with
                                 MergedRows = subTitleHeight
                                 MergedColumns = subTitleWidth
                                 Data = fit (subTitleWidthPx, subTitleHeightPx) cell.Data |> updateTitlePageFont })
    | Sdm.Patterns.Contains Styles.speaker ->
        let speakerHeight = int (float height * 0.06)
        let speakerWidth = int (float width * 0.2)
        let speakerHeightPx = speakerHeight * heightUnit
        let speakerWidthPx = speakerWidth * widthUnit
        let offset = (height / 2) + (height / 4)
        cells |> List.map (fun cell ->
                             { cell with
                                 Row = offset
                                 MergedRows = speakerHeight
                                 MergedColumns = speakerWidth
                                 Data = fit (speakerWidthPx, speakerHeightPx) cell.Data |> updateTitlePageFont })
    | Sdm.Patterns.Contains Styles.date ->
        let dateHeight = int (float height * 0.06)
        let dateWidth = int (float width * 0.2)
        let dateHeightPx = dateHeight * heightUnit
        let dateWidthPx = dateWidth * widthUnit
        cells |> List.map (fun cell ->
                             { cell with
                                 MergedRows = dateHeight
                                 MergedColumns = dateWidth
                                 Data = fit (dateWidthPx, dateHeightPx) cell.Data |> updateTitlePageFont })
    | Sdm.Patterns.Contains Styles.title ->
        let vOffset = int (float height / 20.0)
        let hOffset = int (float width / 25.0)
        let titleHeight = int (float height * 0.12)
        let titleWidth = int (float width * 0.9)
        let titleHeightPx = titleHeight * heightUnit
        let titleWidthPx = titleWidth * widthUnit
        cells |> List.map (fun cell ->
                             { cell with
                                 Row = vOffset; MergedRows = titleHeight
                                 Column = hOffset; MergedColumns = titleWidth
                                 Data = fit (titleWidthPx, titleHeightPx) cell.Data |> updateHeadingFont })
    | _ -> cells
  override __.ArroundParagraph(start, groups, cells) =
    match groups |> Seq.map (fun g -> g :> Sdm.StyleGroup) with
    | Sdm.Patterns.ContainsPrefix "Image" path ->
        let row = int (float height / 20.0 * 9.0)
        imageRow.[path] <- start.Start.Row
        [ Cell.create (row, 1) (1, 1) Format.defaultTextFormat (Other "") ]
    | _ -> cells
  override __.ArroundListItem(start, groups, cells) =
    let h = int (float height * 0.1)
    let w = int (float width * 0.9)
    let hPx = h * heightUnit
    let wPx = w * widthUnit
    cells |> List.mapi (fun i cell ->
                          { cell with
                              Row = cell.Row + (if i <> 0 then h - 1 else 0); MergedRows = h
                              Data = if i <> 0 then cell.Data |> RichText.edit (fun txt -> { txt with FontInfo = txt.FontInfo |> Font.updateSize (size txt.FontInfo * 0.9) })
                                     else fit (wPx, hPx) cell.Data |> RichText.editSegments (fun segs -> { Value = "・"; FontInfo = NoFontInfo }::segs)  })
  override __.ArroundList(start, groups, cells) =
    let hOffset = int (float width / 25.0)
    let h = int (float height * 0.1)
    let w = int (float width * 0.9)
    cells |> List.map (fun cell ->
                         { cell with
                             Column = hOffset + cell.Column; MergedColumns = (w - cell.Column) })
  override __.ArroundTableCell(start, groups, cells) = cells
  override __.ArroundTableColumn(start, headerRange, groups, cells) = cells
  override __.ArroundTableRow(start, headerRange, groups, cells) = cells
  override __.ArroundTable(start, groups, cells) = cells
  override __.Drawing(groups) =
    match groups |> Seq.map (fun g -> g :> Sdm.StyleGroup) with
    | Sdm.Patterns.ContainsPrefix "Image" path ->
        // TODO : pixelの部分をちゃんと計算する
        [ Image.createFromPath path (System.IO.FileInfo(path)) (RowColAndOffsetPixcel ({ Address = imageRow.[path]; Offset = 10<pixel> }, { Address = width / 2; Offset = -100<pixel> }))
          |> Drawing.updateSize (Percent 150) ]
    | _ -> []

  member __.AddKotori(sheet) =
    // TODO : 色々まじめに計算する
    let div = 2
    let originWidth = 1362
    let originHeight = 1533

    let rowAndOffset = { Address = height; Offset = -(originHeight / div / 3) * 1<pixel> }
    let colAndOffset = { Address = width; Offset = -(originWidth / div / 3) * 1<pixel> }
    let kotori =
      Image.createFromPath (i <- i + 1; string i) (System.IO.FileInfo("mini.png")) (Position.RowColAndOffsetPixcel (rowAndOffset, colAndOffset))
      |> Drawing.updateSize (Percent (100 / div))
    { sheet with
        Drawings = kotori::sheet.Drawings }
