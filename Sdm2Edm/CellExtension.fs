namespace Sdm2Edm

open Edm

module Cell =
  let cellToRange (cell: Cell) =
    let startAddr = { Address.Row = cell.Row; Column = cell.Column }
    let endAttr = { Address.Row = cell.Row + cell.MergedRows - 1; Column = cell.Column + cell.MergedColumns - 1 }
    (startAddr, endAttr)

module Cells =
  let calcRange (cells: Cell list) =
    let initRange = Cell.cellToRange cells.Head
    cells
    |> List.fold (fun ((startAddr: Address), (endAddr: Address)) cell ->
         let startAddr =
           { Address.Row = min startAddr.Row cell.Row
             Column = min startAddr.Column cell.Column }
         let endAddr =
           { Address.Row = max endAddr.Row (cell.Row + cell.MergedRows - 1)
             Column = max endAddr.Column (cell.Column + cell.MergedColumns - 1) }
         (startAddr, endAddr)
       ) initRange
    |> fun (s, e) -> { Start = s; End = e }

  let moveDown rows (cells: Cell list) =
    cells |> List.map (fun cell -> { cell with Row = cell.Row + rows }) |> dup |> mapSnd calcRange

  let moveRight cols (cells: Cell list) =
    cells |> List.map (fun cell -> { cell with Column = cell.Column + cols }) |> dup |> mapSnd calcRange
