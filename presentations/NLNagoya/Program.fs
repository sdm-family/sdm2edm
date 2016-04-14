open Sdm2Edm
open Edm.Writer.EPPlus

open NLNagoya
open System

let width = 74
let height = 37

let pages =
  [ yield TitlePage { MainTitle = "No more Legacy documents"
                      SubTitle = "Excel方眼紙でプレゼン"
                      Speaker = "bleis-tift"
                      Date = DateTime(2016, 4, 16) }
    yield ImageAndListPage { Heading = "自己紹介"
                             ImagePath = "at.png"
                             Items = [ TextListItem "id:bleis-tift / @bleis"
                                       TextListItem "株式会社オンザロード"
                                       TextListItem "F#が好き"
                                       TextListItem "Excel方眼紙は嫌い" ] }
    yield ListPage { Heading = "今日話すこと"
                     Items = [ TextListItem "関数型プログラミング言語を使おう！"
                               Nested ("関数型プログラミング言語による身近な問題の解決",
                                       [ TextListItem "あいつを倒す・・・倒したい"
                                         TextListItem "ざっくり仕組みの説明" ])
                               TextListItem "応用例の紹介"
                               TextListItem "まとめ" ] }

    yield SectionPage { SectionName = "関数型プログラミング言語を使おう！" }

    yield SectionPage { SectionName = "関数プログラミング言語による身近な問題の解決" }
    for i in 0..3 do
      yield ListPage { Heading = "身近な問題の例"
                       Items = List.init i (fun _ -> TextListItem "Excel方眼紙") }
    yield ListPage { Heading = "ニッポンの素敵なExcel方眼紙"
                     Items = [ TextListItem "todo" ] }

    yield SectionPage { SectionName = "応用例の紹介" }

    yield SectionPage { SectionName = "まとめ" }
    yield ListPage { Heading = "まとめ"
                     Items = [ TextListItem "TODO" ] } ]

// プレゼン用の設定
let bookSettings =
  { ShowHorizontalScrollBar = Some false
    ShowVerticalScrollBar = Some false
    ShowSheetTabs = Some false }
let sheetSettings =
  { ShowGuideLines = Some false
    ShowHeaders = Some false
    ZoomScale = Some 100<Edm.pixel>
    LongEdge = Some LEWidth
    PrintArea = Some { StartRow = 0; StartColumn = 0; EndRow = height; EndColumn = width }
    Fit = Some FitToPage }

[<EntryPoint>]
let main argv =
  // SDMに変換(自分で用意)
  let sdm = pages |> PresentationPage.toSdm

  // EDMに変換(変換ルールだけ自分で用意)
  let rule = ConvertionRule(width, height)
  let sheets =
    sdm
    |> Converter.convert rule
    |> List.mapi (fun i x -> if i = 1 then x else rule.AddKotori(x))

  // Let's 方眼紙!!!(ライブラリがやってくれる)
  let writer = EPPlusWriter.createWithSettings (bookSettings, sheetSettings) ("NL名古屋.xlsx", "template.xlsx")
  writer.Write(sheets)
  0
