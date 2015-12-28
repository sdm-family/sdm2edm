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

type TableConverter (cells: (int * int * Cell) list) =
  let dataWithIndexingByRow = ResizeArray<ResizeArray<MutableCells>>()
  let dataWithIndexingByCol = ResizeArray<ResizeArray<MutableCells>>()

  let mutable adjustedRows = [||]

  do
    for r, c, cell in cells do
      let cell = MutableCell(cell)
      match dataWithIndexingByRow.TryGet(r) with
      | None ->
          let cols = ResizeArray([| { Values = ResizeArray([|cell|]) } |])
          dataWithIndexingByRow.Add(cols)
      | Some cols ->
          match cols.TryGet(c) with
          | None -> cols.Add({ Values = ResizeArray([|cell|]) })
          | Some cells -> cells.Values.Add(cell)
      
      match dataWithIndexingByCol.TryGet(c) with
      | None ->
          let rows = ResizeArray([| { Values = ResizeArray([|cell|]) } |])
          dataWithIndexingByCol.Add(rows)
      | Some rows ->
          match rows.TryGet(r) with
          | None -> rows.Add({ Values = ResizeArray([|cell|]) })
          | Some cells -> cells.Values.Add(cell)

    // colIdごとのずらした総行数を保持する配列の初期値を設定
    adjustedRows <- Array.zeroCreate dataWithIndexingByCol.Count

  let adjustedRows = adjustedRows

  member __.Rows = dataWithIndexingByRow.Count
  member __.Columns = dataWithIndexingByCol.Count

  member internal __.SetAdjustedRowForTest(colId, newRowId) = adjustedRows.[colId] <- newRowId
  member internal __.GetAdjustedRowsForTest = List.ofArray adjustedRows

  /// 指定したrowを持つセルのRowの値に、対応する列で伸ばした総行数を加算します。
  member __.AdjustRowAddress(row) =
    let cols = dataWithIndexingByRow.[row]
    for colId in 0..(cols.Count - 1) do
      let rows = adjustedRows.[colId]
      for cell in cols.[colId].Values do
        cell.Row <- cell.Row + rows

  member internal __.MaxRowAddress(row) =
    let cols = dataWithIndexingByRow.[row]
    cols |> Seq.collect (fun col -> col.Values |> Seq.map (fun cell -> cell.Row + cell.MergedRows)) |> Seq.max 

  /// 行テーブルの行ごとの高さを統一するために、指定したrowを持つセルの一列一列に対して、その列内の最後の行アドレス(Row)を持つセルのMergedRowsを最大行アドレスまで伸ばします。
  member this.ExtendRowEndToUnify(row) =
    let maxRowAddr = this.MaxRowAddress(row)
    let cols = dataWithIndexingByRow.[row]
    for colId in 0..(cols.Count - 1) do
      // 実セルの最後のセル(=実列アドレスが同じもののうち一番大きい行アドレスを持つセル)のみ、行アドレスを伸ばす対象にする
      // でないと、同一実アドレスに複数のセルがあった際に重なり合ってしまってEdm側でエラーになってしまう
      let targets =
        cols.[colId].Values
        |> Seq.groupBy (fun cell -> cell.Column)
        |> Seq.map (snd >> Seq.last)

      let extends = seq {
        for target in targets do
          let originalMergedRows = target.MergedRows
          // ここで副作用によって最大行アドレス(maxRowAddr)まで伸ばしていることに注意！
          target.MergedRows <- target.MergedRows + (maxRowAddr - target.MergedRows) - target.Row
          yield target.MergedRows - originalMergedRows
      }
      // 同じcolIdを持つセルの伸ばした中で最も短い部分が次の行に波及する行数になる
      adjustedRows.[colId] <- adjustedRows.[colId] + (extends |> Seq.min)

  member __.Cells =
    dataWithIndexingByRow
    |> Seq.collect (fun cols ->
         cols |> Seq.collect (fun col -> col.Values))
    |> Seq.sortBy (fun cell -> (cell.Row, cell.Column))
    |> Seq.map (fun cell -> cell.ToCell())
    |> Seq.toList