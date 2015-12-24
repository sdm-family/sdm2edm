namespace Sdm2Edm

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

type ConvertedResult = Cell list

[<AbstractClass>]
type ConvertionRule () =
  /// 見出しを変換し、次のセルのアドレスと変換結果を返します。
  abstract Heading: address:Address * groups:TextStyleGroup list * level:int * text:Text -> Address * ConvertedResult
  /// 段落を変換し、次のセルのアドレスと変換結果を返します。
  abstract Paragraph: address:Address * groups:TextStyleGroup list * lines:Text list -> Address * ConvertedResult
  /// リスト全体を変換し、次のセルのアドレスと変換結果を返します。
  /// 引数cellsには変換されたすべてのリストの項目がCellのリストとして渡されます。
  abstract List: startAddress:Address * endAddress:Address * groups:ListStyleGroup list * cells:Cell list -> Address * ConvertedResult
  /// リストの項目を変換し、次のセルのアドレスと変換結果を返します。
  /// 引数itemCellsには変換されたリストの項目がCellのリストとして渡されます。
  abstract ListItem: address:Address * groups:ListStyleGroup list * itemCells:Cell list -> Address * ConvertedResult