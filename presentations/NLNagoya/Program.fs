open Sdm2Edm
open Edm.Writer.EPPlus

open NLNagoya
open System

let width = 74
let height = 41

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
    yield ListPage { Heading = "関数型プログラミング言語？"
                     Items = [ Nested ("手続きを書くのではなく、関数を組み合わせる",
                                       [ TextListItem "関数を値として使える" ])
                               Nested ("ループより再帰",
                                       [ TextListItem "再帰よりも高階関数" ])
                               TextListItem "(静的な型を持っている場合)パターンマッチ等々" ] }
    yield ListPage { Heading = "何に使えるの？"
                     Items = [ Nested ("なんにでも使える",
                                       [ TextListItem "JavaやC#でできることはできる" ])
                               TextListItem "この問いは適切ではない" ] }
    yield ListPage { Heading = "より適切な問い"
                     Items = [ TextListItem "何が得意なの？" ] }
    yield ListPage { Heading = "得意なこと(一例)"
                     Items = [ Nested ("言語処理系",
                                       [ TextListItem "F#のコンパイラはF#で書かれている" ])
                               Nested ("木構造の分解と再構築",
                                       [ TextListItem "木の回転"
                                         TextListItem "構文木の走査、変換" ])
                               Nested ("テスト容易性の確保のしやすさ",
                                       [ TextListItem "副作用をなくす方向に力が働く" ])
                               Nested ("金融系",
                                       [ TextListItem "やったことないのでノーコメント" ]) ] }

    yield SectionPage { SectionName = "関数型プログラミング言語による身近な問題の解決" }
    yield ListPage { Heading = "身近な問題の解決"
                     Items = [ Nested ("関数型プログラミング言語をより身近に感じてもらう",
                                       [ TextListItem "わかりやすさ"
                                         TextListItem "説得力"
                                         TextListItem "(インパクト)" ]) ] }
    for i in 0..4 do
      yield ListPage { Heading = "身近な問題の例"
                       Items = List.init i (fun i -> TextListItem ("Excel方眼紙" + String.replicate i "!")) }
    yield ListPage { Heading = "ニッポンの素敵なExcel方眼紙"
                     Items = [ Nested ("SIer御用達",
                                       [ TextListItem "Webの人はごめんなさい" ])
                               TextListItem "表計算ではなくレイアウトツールとしてのExcel"
                               TextListItem "倒したい" ] }
    yield ListPage { Heading = "Excel方眼紙の何が悪いか"
                     Items = [ TextListItem "バージョン管理が面倒"
                               TextListItem "差分が見れない"
                               TextListItem "編集が面倒" ] }
    yield ListPage { Heading = "Excel方眼紙の何が悪いか"
                     Items = [ TextListItem "バージョン管理が面倒"
                               TextListItem "差分が見れない"
                               TextListItem "編集が面倒"
                               TextListItem "Esc押すと入力したものが消える" ] }
    yield ListPage { Heading = "Excel方眼紙を倒す前提"
                     Items = [ Nested ("Excel方眼紙は最終出力と割り切る",
                                       [ TextListItem "生成されたファイルを弄ってはいけない"
                                         TextListItem "弄ったら(弄られたら)負け" ])
                               Nested ("最終出力なので・・・",
                                       [ TextListItem "汎用のExcel操作は不要"
                                         TextListItem "読み込みも不要"
                                         TextListItem "モデルを構築したら後は書き出すだけ" ]) ] }
    yield ListPage { Heading = "この資料を出力したツール"
                     Items = [ Nested ("Sdm / Edm / Sdm2Edm",
                                       [ TextListItem "最終的にはEPPlus" ])
                               TextListItem "木構造をごにょごにょして最終的にExcel方眼紙を出力"
                               TextListItem "当然(?)F#製" ] }
    yield ShapePage { Heading = "ざっくり"
                      ShapeDescription = "Overview" }
    yield ListPage { Heading = "登場人物"
                     Items = [ TextListItem "Sdm : 構造化された文書のモデル。抽象度高い"
                               Nested ("Edm : Excel文書のモデル。セルと一対一対応",
                                       [ TextListItem "Edm.Writer.EPPlus : EdmをEPPlusを使ってExcelファイルに出力。hogehogehogehoge" ])
                               TextListItem "Sdm2Edm : SdmをEdmに変換する仕組みを提供。" ] }
    yield ListPage { Heading = "EPPlus直接使うんじゃダメ？"
                     Items = [ Nested ("抽象度を分ける",
                                       [ TextListItem "文書の構造と表現を分ける"
                                         TextListItem "HTMLとCSSみたいな？" ])
                               Nested ("セルのアドレス計算からの解放",
                                       [ TextListItem "Sdm2Edmがある程度やってくれる"
                                         TextListItem "論理的なページ機能もある" ]) ] }
    yield CodePage { Heading = "Sdm概要"
                     Code = """
// 今のところ画像には非対応
type Component =
  | Heading of TextStyleGroup list * Level:int * Value:Text
  | Paragraph of TextStyleGroup list * Lines: Text list
  | List of ListStyleGroup list * Items: Component list
  | Table of TableStyleGroup list * TableContents

type Page = { Name: string; Components: Component list }""".TrimStart() }
    yield CodePage { Heading = "Edm概要"
                     Code = """
type Cell =
  { Row: int; Column: int
    // マージもカンタン
    MergedRows: int; MergedColumns: int
    Format: FormatInfo
    Data: Data }
type Sheet =
  { Name: string
    Cells: Cell list
    Drawings: Drawing list }""".TrimStart() }
    yield CodePage { Heading = "Sdm2Edm概要"
                     Code = """
// 実際はデフォルト実装付きのクラスを継承する方が便利
// Textだけを実装すればいいConvertionRule抽象クラスと、
// 何も実装しなくていいSimpleConvertionRuleクラスが用意されている
type IConvertionRule =
  abstract Text:
    ComponentRange * TextStyleGroup list * Text -> ConvertedResult
  abstract ArroundHeading:
    ComponentRange * TextStyleGroup list * int * Cell list -> ConvertedResult
  (* snip *)
  abstract Drawing:
    groups:TextStyleGroup list -> Drawing list""".TrimStart() }
    yield ListPage { Heading = "Sdm2Edmでできること"
                     Items = [ Nested ("SdmからEdmに変換",
                                       [ TextListItem "何も実装しなくてもOK"
                                         TextListItem "カスタマイズもある程度可能" ])
                               Nested ("Edmのページ処理",
                                       [ TextListItem "Edm to Edm"
                                         TextListItem "あふれたら新しいシートを作るPager"
                                         TextListItem "他のPagerも簡単に作れる" ]) ] }

    yield SectionPage { SectionName = "応用例の紹介" }
    yield ListPage { Heading = "応用例"
                     Items = [ TextListItem "プレゼン資料作成"
                               TextListItem "内部DSLからのメッセージ仕様書作成"
                               TextListItem "外部DSLからのデータベース仕様書生成"
                               TextListItem "Markdownから仕様書作成"
                               TextListItem "などなど" ] }
    yield ListPage { Heading = "プレゼン資料作成"
                     Items = [ TextListItem "発表のための一発ネタ"
                               Nested ("ただ、PowerPointより優れた点も・・・",
                                       [ TextListItem "コードの色付け"
                                         TextListItem "テキストで管理できる" ]) ] }
    yield ListPage { Heading = "DSLから仕様書作成"
                     Items = [ TextListItem "社内のツールとして"
                               TextListItem "SIerなら何かしらあるのでは？"
                               Nested ("提出形式としてのExcel方眼紙",
                                       [ TextListItem "万が一提出形式が変わっても何とかなる(多分)" ]) ] }
    yield ListPage { Heading = "Markdownから仕様書作成"
                     Items = [ TextListItem "(未完)" ] }

    yield SectionPage { SectionName = "まとめ" }
    yield ListPage { Heading = "まとめ"
                     Items = [ TextListItem "関数型プログラミング言語は実用的！"
                               TextListItem "身のまわりのExcel方眼紙を倒していこう！"
                               TextListItem "テキスト等からExcel方眼紙を生成！" ] }
    yield ListPage { Heading = "まとめ"
                     Items = [ TextListItem "関数型プログラミング言語は実用的！"
                               TextListItem "身のまわりのExcel方眼紙を倒していこう！"
                               TextListItem "テキスト等からExcel方眼紙を生成！"
                               TextListItem "Excelはプレゼンソフトじゃない！" ] } ]

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
