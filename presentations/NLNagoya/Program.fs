open Sdm2Edm
open Edm.Writer.EPPlus

open NLNagoya
open System

let width = 79
let height = 36

let pages =
  [ TitlePage { MainTitle = "No more Legacy documents"
                SubTitle = "Excel方眼紙でプレゼン"
                Speaker = "bleis-tift"
                Date = DateTime(2016, 4, 16) }
    ImageAndListPage { Heading = "自己紹介"
                       ImagePath = "at.png"
                       Items = [ TextListItem "id:bleis-tift / @bleis"
                                 TextListItem "株式会社オンザロード"
                                 TextListItem "F#が好き"
                                 TextListItem "Excel方眼紙は嫌い" ] }
    ListPage { Heading = "今日話すこと"
               Items = [ TextListItem "関数型プログラミング言語を使おう！"
                         Nested ("関数型プログラミング言語による身近な問題の解決",
                                 [ TextListItem "あいつを倒す・・・倒したい"
                                   TextListItem "ざっくり仕組みの説明" ])
                         TextListItem "応用例の紹介" ] } ]

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
  let settings = { ShowGuideLines = Some false
                   LongEdge = Some LEWidth
                   PrintArea = Some { StartRow = 0; StartColumn = 0; EndRow = height; EndColumn = width }
                   Fit = Some FitToPage }
  let writer = EPPlusWriter.createWithSettings settings ("NL名古屋.xlsx", "template.xlsx")
  writer.Write(sheets)
  0
