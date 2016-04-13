namespace NLNagoya

open Edm
open Microsoft.FSharp.Compiler.SourceCodeServices
open System

module CodeColorizer =
  let rec private tokenizeLine (tokenizer: FSharpLineTokenizer) state =
    match tokenizer.ScanToken(state) with
    | Some tok, state ->
        let res, state = tokenizeLine tokenizer state
        (tok::res, state)
    | None, state -> ([], state)

  let rec private tokenizeLines (sourceTok: FSharpSourceTokenizer) state count lines =
    match lines with
    | [] -> Seq.empty
    | line::lines ->
        let tokenizer = sourceTok.CreateLineTokenizer(line)
        let res, state = tokenizeLine tokenizer state
        seq {
          yield! res |> List.map (fun x -> (x, count))
          yield! tokenizeLines sourceTok state (count + 1) lines
        }

  let private grouping (sourceTok: FSharpSourceTokenizer) (txt: RichText) =
    let lines =
      (txt.Segments |> List.map (fun seg -> seg.Value) |> String.concat "").Split([|"\r\n";"\r";"\n"|], StringSplitOptions.None)
    let toks = tokenizeLines sourceTok 0L 1 (List.ofArray lines)
    let mutable prevLine = 1
    [ for tok, lineNo in toks do
        if prevLine <> lineNo then
          yield ("\n", FSharpTokenColorKind.Default)
          prevLine <- lineNo
        let line = lines.[lineNo - 1]
        let str = line.Substring(tok.LeftColumn, tok.RightColumn - tok.LeftColumn + 1)
        yield (str, tok.ColorClass) ]

  let private toSegment (str, colorClass) =
    let font color = Font.create (FontName "メイリオ") (NoFontSize) NoFontStyle color
    { RichTextSegment.Value = str
      FontInfo =
        match colorClass with
        | FSharpTokenColorKind.Default -> font RgbColor.black
        | FSharpTokenColorKind.UpperIdentifier -> font RgbColor.black
        | FSharpTokenColorKind.Number -> font RgbColor.black
        | FSharpTokenColorKind.String -> font (RgbColor.create (163, 21, 21))
        | FSharpTokenColorKind.Keyword -> font (RgbColor.create (0, 0, 255))
        | FSharpTokenColorKind.PreprocessorKeyword -> font (RgbColor.create (0, 0, 255))
        | FSharpTokenColorKind.Operator -> font (RgbColor.create (255, 0, 0))
        | FSharpTokenColorKind.Identifier -> font RgbColor.black
        | FSharpTokenColorKind.InactiveCode -> font (RgbColor.create (157, 157, 157))
        | FSharpTokenColorKind.Comment -> font (RgbColor.create (0, 128, 0)) }

  let private f (sourceTok: FSharpSourceTokenizer) (txt: RichText) =
    { txt with
        Segments = txt |> grouping sourceTok |> List.map toSegment }

  let color (cell: Cell) =
    let sourceTok = FSharpSourceTokenizer([], "codesnip.fs")
    { cell with Data = cell.Data |> Data.editRichText (f sourceTok) }