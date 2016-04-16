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
          let nlCount = lineNo - prevLine
          yield (String.replicate nlCount "\n", FSharpTokenColorKind.Default)
          prevLine <- lineNo
        let line = lines.[lineNo - 1]
        let str = line.Substring(tok.LeftColumn, tok.RightColumn - tok.LeftColumn + 1)
        yield (str, tok.ColorClass) ]

  type private TokKind =
    | TypeKeyword
    | DUBar
    | TypeAnot
    | Other

  let mutable private prevTokKind = Other
  let mutable private prevStr = ""

  let private toSegment (str, colorClass) =
    if prevTokKind = TypeAnot then
      match str with
      | ";" -> prevTokKind <- Other
      | "\n" when prevStr <> ":" -> prevTokKind <- Other
      | _ -> ()
    if not (String.IsNullOrWhiteSpace(str)) then
      prevStr <- str
    let font color = Font.create (FontName "MeiryoKe_Console") (NoFontSize) NoFontStyle color
    { RichTextSegment.Value = str
      FontInfo =
        match colorClass with
        | FSharpTokenColorKind.Default ->
            if str = "|" then prevTokKind <- DUBar
            font RgbColor.black
        | FSharpTokenColorKind.UpperIdentifier ->
            match prevTokKind with
            | TypeKeyword -> prevTokKind <- Other; font (RgbColor.create (43, 145, 175))
            | TypeAnot -> prevTokKind <- TypeAnot; font (RgbColor.create (43, 145, 175))
            | DUBar -> prevTokKind <- Other; font (RgbColor.create (255, 128, 0))
            | _ -> font RgbColor.black
        | FSharpTokenColorKind.Identifier ->
            match prevTokKind with
            | TypeAnot -> font (RgbColor.create (43, 145, 175))
            | _ -> font RgbColor.black
        | FSharpTokenColorKind.Number -> font RgbColor.black
        | FSharpTokenColorKind.String -> font (RgbColor.create (163, 21, 21))
        | FSharpTokenColorKind.Keyword ->
            if str = "type" then prevTokKind <- TypeKeyword
            elif str = "of" then prevTokKind <- TypeAnot
            font (RgbColor.create (0, 0, 255))
        | FSharpTokenColorKind.PreprocessorKeyword -> font (RgbColor.create (0, 0, 255))
        | FSharpTokenColorKind.Operator ->
            if str = ":" then prevTokKind <- TypeAnot
            font (RgbColor.create (255, 0, 0))
        | FSharpTokenColorKind.InactiveCode -> font (RgbColor.create (157, 157, 157))
        | FSharpTokenColorKind.Comment -> font (RgbColor.create (0, 128, 0)) }

  let private f (sourceTok: FSharpSourceTokenizer) (txt: RichText) =
    { txt with
        Segments = txt |> grouping sourceTok |> List.map toSegment }

  let color (cell: Cell) =
    let sourceTok = FSharpSourceTokenizer([], "codesnip.fs")
    { cell with Data = cell.Data |> Data.editRichText (f sourceTok) }