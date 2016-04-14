namespace Sdm2Edm

open Sdm
open Edm

type ConvertedResult = Cell list

type IConvertionRule =
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

  /// セル全体の変換規則です。
  abstract ArroundTableCell: start:ComponentRange * groups:CellStyleGroup list * cells:Cell list -> ConvertedResult

  /// 行ベースのテーブル1列全体の変換規則です。
  abstract ArroundTableColumn: start:ComponentRange * headerRange:ComponentRange * groups:ColumnStyleGroup list * cells:Cell list -> ConvertedResult

  /// 列ベースのテーブル1行全体の変換規則です。
  abstract ArroundTableRow: start:ComponentRange * headerRange:ComponentRange * groups:RowStyleGroup list * cells:Cell list -> ConvertedResult

  /// テーブル全体の変換規則です。
  abstract ArroundTable: start:ComponentRange * groups:TableStyleGroup list * cells:Cell list -> ConvertedResult

  /// Drawingへの変換規則です。
  abstract Drawing: groups:TextStyleGroup list -> Drawing list

[<AbstractClass>]
type ConvertionRule () =
  /// テキストの変換規則です。
  abstract Text: start:ComponentRange * groups:TextStyleGroup list * text:Text -> ConvertedResult

  /// 見出し全体の変換規則です。
  abstract ArroundHeading: start:ComponentRange * groups:TextStyleGroup list * level:int * cells:Cell list -> ConvertedResult
  default __.ArroundHeading(_, _, _, cells) = cells

  /// 段落全体の変換規則です。
  abstract ArroundParagraph: start:ComponentRange * groups:TextStyleGroup list * cells:Cell list -> ConvertedResult
  default __.ArroundParagraph(_, _, cells) = cells

  /// リストの項目全体の変換規則です。
  abstract ArroundListItem: start:ComponentRange * groups:ListStyleGroup list * cells:Cell list -> ConvertedResult
  default __.ArroundListItem(_, _, cells) = cells

  /// リスト全体の変換規則です。
  abstract ArroundList: start:ComponentRange * groups:ListStyleGroup list * cells:Cell list -> ConvertedResult
  default __.ArroundList(_, _, cells) = cells

  /// セル全体の変換規則です。
  abstract ArroundTableCell: start:ComponentRange * groups:CellStyleGroup list * cells:Cell list -> ConvertedResult
  default __.ArroundTableCell(_, _, cells) = cells

  /// 行ベースのテーブル1列全体の変換規則です。
  abstract ArroundTableColumn: start:ComponentRange * headerRange:ComponentRange * groups:ColumnStyleGroup list * cells:Cell list -> ConvertedResult
  default __.ArroundTableColumn(_, _, _, cells) = cells

  /// 列ベースのテーブル1行全体の変換規則です。
  abstract ArroundTableRow: start:ComponentRange * headerRange:ComponentRange * groups:RowStyleGroup list * cells:Cell list -> ConvertedResult
  default __.ArroundTableRow(_, _, _, cells) = cells

  /// テーブル全体の変換規則です。
  abstract ArroundTable: start:ComponentRange * groups:TableStyleGroup list * cells:Cell list -> ConvertedResult
  default __.ArroundTable(_, _, cells) = cells

  /// Drawingへの変換規則です。
  abstract Drawing: groups:TextStyleGroup list -> Drawing list
  default __.Drawing(_) = []

  interface IConvertionRule with
    override this.Text(start, groups, text) = this.Text(start, groups, text)
    override this.ArroundHeading(start, groups, level, cells) = this.ArroundHeading(start, groups, level, cells)
    override this.ArroundList(start, groups, cells) = this.ArroundList(start, groups, cells)
    override this.ArroundListItem(start, groups, cells) = this.ArroundListItem(start, groups, cells)
    override this.ArroundParagraph(start, groups, cells) = this.ArroundParagraph(start, groups, cells)
    override this.ArroundTable(start, groups, cells) = this.ArroundTable(start, groups, cells)
    override this.ArroundTableCell(start, groups, cells) = this.ArroundTableCell(start, groups, cells)
    override this.ArroundTableColumn(start, headerRange, groups, cells) = this.ArroundTableColumn(start, headerRange, groups, cells)
    override this.ArroundTableRow(start, headerRange, groups, cells) = this.ArroundTableRow(start, headerRange, groups, cells)
    override this.Drawing(groups) = this.Drawing(groups)

type SimpleConvertionRule () =
  inherit ConvertionRule ()

  override __.Text(start, _, text) =
    Converters.Text.toOneRichTextCell start.Start text
