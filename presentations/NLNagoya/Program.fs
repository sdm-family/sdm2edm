open Sdm2Edm
open Edm.Writer.EPPlus

open NLNagoya
open System

let width = 94
let height = 45

let pages =
  [ TitlePage { MainTitle = "No more Legacy documents"
                SubTitle = "Excel方眼紙でプレゼン"
                Speaker = "bleis-tift"
                Date = DateTime(2016, 4, 16) } ]

[<EntryPoint>]
let main argv =
  // SDMに変換(自分で用意)
  let sdm = pages |> PresentationPage.toSdm

  // EDMに変換(変換ルールだけ自分で用意)
  let rule = ConvertionRule(width, height)
  let sheets =
    sdm
    |> Converter.convert rule
    |> List.map rule.AddKotori

  // Let's 方眼紙!!!(ライブラリがやってくれる)
  let writer = EPPlusWriter.create ("NL名古屋.xlsx", "template.xlsx")
  writer.Write(sheets)
  0
