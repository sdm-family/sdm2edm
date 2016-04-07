namespace NLNagoya

open Sdm2Edm
open Edm

type ConvertionRule(width: int, height: int) =
  inherit Sdm2Edm.ConvertionRule()

  let heightUnit = 22<pixel>
  let widthUnit = 20<pixel>

  let mutable i = 0

  // TODO : Sdm2Edmに移動
  let toEdmSegment (seg: Sdm.TextSegment) =
    { RichTextSegment.Value = seg.Value; FontInfo = Font.noSpecific }

  let fit (boxWidth, boxHeight) data =
    // boxHeightを基準にフォントサイズを決定
    let fontSize = 0.364 * (float boxHeight) |> int |> float
    // TODO : はみ出るようならboxWidthを基準にフォントサイズを決定
    data
    |> Data.editRichText (fun txt -> { txt with FontInfo = txt.FontInfo |> Font.updateSize fontSize |> Font.updateName "Yu Gothic" })

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
                                 Data = fit (titleWidthPx, titleHeightPx) cell.Data })
    | Sdm.Patterns.Contains Styles.subTitle ->
        let subTitleHeight = int (float height * 0.07)
        let subTitleWidth = int (float width * 0.75)
        let subTitleHeightPx = subTitleHeight * heightUnit
        let subTitleWidthPx = subTitleWidth * widthUnit
        cells |> List.map (fun cell ->
                             { cell with
                                 MergedRows = subTitleHeight
                                 MergedColumns = subTitleWidth
                                 Data = fit (subTitleWidthPx, subTitleHeightPx) cell.Data })
    | Styles.Heading.Speaker ->
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
                                 Data = fit (speakerWidthPx, speakerHeightPx) cell.Data })
    | Styles.Heading.Date ->
        let dateHeight = int (float height * 0.06)
        let dateWidth = int (float width * 0.2)
        let dateHeightPx = dateHeight * heightUnit
        let dateWidthPx = dateWidth * widthUnit
        cells |> List.map (fun cell ->
                             { cell with
                                 MergedRows = dateHeight
                                 MergedColumns = dateWidth
                                 Data = fit (dateWidthPx, dateHeightPx) cell.Data })
    | Styles.Heading.Title ->
        cells
    | _ -> cells
  override __.ArroundParagraph(start, groups, cells) = cells
  override __.ArroundListItem(start, groups, cells) = cells
  override __.ArroundList(start, groups, cells) = cells 
  override __.ArroundTableCell(start, groups, cells) = cells
  override __.ArroundTableColumn(start, headerRange, groups, cells) = cells
  override __.ArroundTableRow(start, headerRange, groups, cells) = cells
  override __.ArroundTable(start, groups, cells) = cells

  member __.AddKotori(sheet) =
    // TODO : 色々まじめに計算する
    let div = 2
    let originWidth = 1362
    let originHeight = 1533

    let rowAndOffset = { Address = height; Offset = -(originHeight / div / 3) * 1<pixel> }
    let colAndOffset = { Address = width; Offset = -(originWidth / div / 3) * 1<pixel> }
    { sheet with
        Drawings =
          [ Image.cerateFromPath (i <- i + 1; string i) (System.IO.FileInfo("mini.png")) (Position.RowColAndOffsetPixcel (rowAndOffset, colAndOffset))
            |> Drawing.updateSize (Percent (100 / div)) ] }
