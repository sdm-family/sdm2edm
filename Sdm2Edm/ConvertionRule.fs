﻿namespace Sdm2Edm

open Sdm
open Edm

type Address = {
  Row: int
  Column: int
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Address =
  let A1 = { Row = 0; Column = 0 }

  let updateRow diff address = { address with Address.Row = address.Row + diff }
  let updateCol diff address = { address with Address.Column = address.Column + diff }

type ComponentRange = {
  Start: Address
  End: Address
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ComponentRange =
  let preFirst = {
    Start = { Row = -1; Column = 0 }
    End = { Row = -1; Column = 0 }
  }

  let A1 = {
    Start = Address.A1
    End = Address.A1
  }

  let nextRowAddress range =
    { Row = range.End.Row + 1; Column = range.Start.Column }

  let nextComponentStart range =
    let address = nextRowAddress range
    { Start = address; End = address }

type ConvertedResult = Cell list

[<AbstractClass>]
type ConvertionRule () =
  /// テキストの変換規則です。
  abstract Text: start:ComponentRange * groups:TextStyleGroup list * text:Text -> ConvertedResult

  /// 見出し全体の変換規則です。
  abstract ArroundHeading: start:ComponentRange * groups:TextStyleGroup list * level:int * cells:Cell list -> ConvertedResult

  /// 段落全体の変換規則です。
  abstract ArroundParagraph: start:ComponentRange * groups:TextStyleGroup list * cells:Cell list -> ConvertedResult

  /// リストの項目全体の変換規則です。
  abstract ArroundListItem: start:ComponentRange * groups:ListStyleGroup list * cells:Cell list -> ConvertedResult

  /// リスト全体の変換規則です。
  abstract ArroundList: start:ComponentRange * groups:ListStyleGroup list * cells:Cell list -> ConvertedResult
