namespace Sdm2Edm

open Edm
open System.Collections.Generic

type MutableCell(cell: Cell) =
  let mutable cell = cell

  member __.Row
    with get ()  = cell.Row
    and  set row = cell <- { cell with Row = row }
  member __.Column
    with get ()  = cell.Column
    and  set col = cell <- { cell with Column = col }
  member __.MergedRows
    with get ()         = cell.MergedRows
    and  set mergedRows = cell <- { cell with MergedRows = mergedRows }
  member __.MergedColumns
    with get ()         = cell.MergedColumns
    and  set mergedCols = cell <- { cell with MergedColumns = mergedCols }

  member __.ToCell() = cell

type MutableCells = {
  Values: ResizeArray<MutableCell>
}

type IndexType = RowIndex | ColIndex

type TableConverter (cells: (int * int * Cell) list, idxType: IndexType) =
  let index = ResizeArray<ResizeArray<MutableCells>>()

  let initIndex () =
    let subKeySet = HashSet<_>()
    for r, c, cell in cells do
      let cell = MutableCell(cell)
      let mainKey, subKey =
        match idxType with
        | RowIndex -> r, c
        | ColIndex -> c, r
      match index.TryGet(mainKey) with
      | None ->
          let xs = ResizeArray([| { Values = ResizeArray([|cell|]) } |])
          index.Add(xs)
      | Some xs ->
          match xs.TryGet(subKey) with
          | None -> xs.Add({ Values = ResizeArray([|cell|]) })
          | Some cells -> cells.Values.Add(cell)
      subKeySet.Add(subKey) |> ignore
    subKeySet.Count

  let mutable adjustedTable = [||]

  do
    let subKeyLen = initIndex ()
    // colIdごとのずらした総行数を保持する配列の初期値を設定
    adjustedTable <- Array.zeroCreate subKeyLen

  let adjustedTable = adjustedTable

  member __.Index = index
  member __.AdjustedTable = adjustedTable

  member __.Rows =
    match idxType with RowIndex -> index.Count | ColIndex -> adjustedTable.Length
  member __.Columns =
    match idxType with ColIndex -> index.Count | RowIndex -> adjustedTable.Length

  member internal __.SetAdjustedDataForTest(colId, newRowId) = adjustedTable.[colId] <- newRowId
  member internal __.GetAdjustedTableForTest = List.ofArray adjustedTable

  /// idで指定したrow/colを持つセルのRow/Columnの値に、対応する列/行で伸ばした総行数/総列数を加算します。
  member __.AdjustAddress(id) =
    let xs = index.[id]
    for i in 0..(xs.Count - 1) do
      let adjusted = adjustedTable.[i]
      for cell in xs.[i].Values do
        match idxType with
        | RowIndex -> cell.Row <- cell.Row + adjusted
        | ColIndex -> cell.Column <- cell.Column + adjusted

  member internal __.MaxAddress(id, (addresser: _ -> int)) =
    let xs = index.[id]
    xs |> Seq.collect (fun x -> x.Values |> Seq.map addresser) |> Seq.max

  member internal this.ExtendToUnify(x, maxAddress, groupingKey) =
    let maxAddr = maxAddress x
    let ys = this.Index.[x]
    for id in 0..(ys.Count - 1) do
      // 実セルの最後のセル(=実列/行アドレスが同じもののうち一番大きい行/列アドレスを持つセル)のみ、行/列アドレスを伸ばす対象にする
      // でないと、同一実アドレスに複数のセルがあった際に重なり合ってしまってEdm側でエラーになってしまう
      let targets =
        ys.[id].Values
        |> Seq.groupBy groupingKey
        |> Seq.map (snd >> Seq.last)

      let extends = seq {
        for target in targets do
          let originalMergedRows = target.MergedRows
          // ここで副作用によって最大行アドレス(maxAddr)まで伸ばしていることに注意！
          target.MergedRows <- maxAddr - target.Row
          yield target.MergedRows - originalMergedRows
      }
      // 同じidを持つセルの伸ばした中で最も短い部分が次の行に波及する行数になる
      this.AdjustedTable.[id] <- this.AdjustedTable.[id] + (extends |> Seq.min)

  member __.Cells =
    index
    |> Seq.collect (fun cols ->
         cols |> Seq.collect (fun col -> col.Values))
    |> Seq.sortBy (fun cell -> (cell.Row, cell.Column))
    |> Seq.map (fun cell -> cell.ToCell())
    |> Seq.toList

type RowsTableConverter(cells: (int * int * Cell) list) =
  inherit TableConverter(cells, RowIndex)

  member internal this.MaxRowAddress(row) =
    this.MaxAddress(row, fun cell -> cell.Row + cell.MergedRows)

  /// 行テーブルの行ごとの高さを統一するために、指定したrowを持つセルの一列一列に対して、その列内の最後の行アドレス(Row)を持つセルのMergedRowsを最大行アドレスまで伸ばします。
  member this.ExtendRowEndToUnify(row) = this.ExtendToUnify(row, this.MaxRowAddress, (fun cell -> cell.Column))
