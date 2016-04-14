namespace NLNagoya

open Sdm2Edm
open Edm

open System
open System.Text

type ConvertionRule(width: int, height: int) =
  inherit Sdm2Edm.SimpleConvertionRule()

  let heightUnit = 22<pixel>
  let widthUnit = 20<pixel>

  let drawingRow = System.Collections.Generic.Dictionary<_, _>()

  let mutable i = 0

  // TODO : Sdmに移動
  let size font =
    match font with
    | FontInfo font -> match font.Size with FontSize size -> size | NoFontSize -> failwith "oops!"
    | NoFontInfo -> failwith "oops!"

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
    |> Data.editRichText (fun txt -> { txt with FontInfo = txt.FontInfo |> Font.editFontInfoData (fun f -> { f with Style = BoldStyle; Underline = Some Underline }) })

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

  let borderBottom (cell: Cell) =
    { cell with Format = { cell.Format with Borders = { cell.Format.Borders with Bottom = { Style = ThickBorder; Color = Rgb (255, 0, 0) } }
                                            Layout = { cell.Format.Layout with VerticalLayout = VLSup WrapText } } }

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
                                 Data = fit (titleWidthPx, titleHeightPx) cell.Data |> updateTitlePageFont |> highlightUpper } |> borderBottom)
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
    | Sdm.Patterns.Contains Styles.sectionName ->
        let hOffset = int (float width / 25.0)
        let titleHeight = int (float height * 0.12)
        let titleWidth = int (float width * 0.9)
        let titleHeightPx = titleHeight * heightUnit
        let titleWidthPx = titleWidth * widthUnit
        cells |> List.map (fun cell ->
                             { cell with
                                 Row = height / 2 - (titleHeight / 2); MergedRows = titleHeight
                                 Column = hOffset; MergedColumns = titleWidth
                                 Data = fit (titleWidthPx, titleHeightPx) cell.Data |> updateHeadingFont })
    | _ -> cells
  override __.ArroundParagraph(start, groups, cells) =
    let fs = Styles.fsharp :> Sdm.StyleGroup
    match groups |> Seq.map (fun g -> g :> Sdm.StyleGroup) with
    | Sdm.Patterns.ContainsPrefix "Image" path ->
        let row = int (float height / 20.0 * 9.0)
        drawingRow.[path] <- start.Start.Row
        [ Cell.create (row, 1) (1, 1) Format.defaultTextFormat (Other "") ]
    | Sdm.Patterns.ContainsPrefix "Shape" description ->
        let row = int (float height / 20.0 * 9.0)
        drawingRow.[description] <- start.Start.Row
        [ Cell.create (row, 1) (1, 1) Format.defaultTextFormat (Other "") ]
    | Sdm.Patterns.Contains fs ->
        let w = int (float width * 0.9)
        cells
        |> List.map CodeColorizer.color
        |> List.map (fun cell ->
                       let rows = height - cell.Row
                       let size = float rows / 2.0
                       let format = { cell.Format with Layout = { cell.Format.Layout with VerticalLayout = VLSup WrapText } }
                       { cell with
                           MergedRows = rows; MergedColumns = (w - cell.Column)
                           Format = format
                           Data = cell.Data |> RichText.edit (fun txt -> { txt with FontInfo = txt.FontInfo |> Font.updateSize size}) })
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
  override __.Drawing(groups) =
    match groups |> Seq.map (fun g -> g :> Sdm.StyleGroup) with
    | Sdm.Patterns.ContainsPrefix "Image" path ->
        // TODO : pixelの部分をちゃんと計算する
        [ Image.createFromPath path (System.IO.FileInfo(path)) (RowColAndOffsetPixcel ({ Address = drawingRow.[path]; Offset = 10<pixel> }, { Address = width / 2; Offset = -100<pixel> }))
          |> Drawing.updateSize (Percent 150) ]
    | Sdm.Patterns.ContainsPrefix "Shape" description ->
        let hOffset = int (float width / 25.0)
        let row = { Address = drawingRow.[description]; Offset = 0<pixel> }
        let col = { Address = hOffset; Offset = 0<pixel> }
        Shapes.shape (RowColAndOffsetPixcel (row, col)) description
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
