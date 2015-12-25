module CellExtensionTest

open Persimmon
open UseTestNameByReflection

open Sdm2Edm

let ``cellToRangeでセルの占める範囲を計算できる``  =
  let test (cell, expected) = test {
    do! assertEquals expected (Cell.cellToRange cell)
  }
  parameterize {
    case (emptyCell (0, 0, 1, 1), range ((0, 0), (0, 0)))
    case (emptyCell (0, 0, 2, 4), range ((0, 0), (1, 3)))
    case (emptyCell (1, 2, 1, 1), range ((1, 2), (1, 2)))
    case (emptyCell (1, 2, 2, 10), range ((1, 2), (2, 11)))
    run test
  }

let ``calcRangeでセル群の占める範囲を計算できる`` =
  let test (cells, expected) = test {
    do! assertEquals expected (Cells.calcRange cells)
  }
  parameterize {
    case ([emptyCell (0, 0, 1, 1)], { Start = addr (0, 0); End = addr (0, 0) })
    case ([emptyCell (0, 0, 2, 5)], { Start = addr (0, 0); End = addr (1, 4) })
    case ([emptyCell (10, 2, 1, 1)], { Start = addr (10, 2); End = addr (10, 2) })
    case ([emptyCell (10, 2, 2, 5)], { Start = addr (10, 2); End = addr (11, 6) })
    case ([emptyCell (0, 0, 1, 1); emptyCell (10, 2, 1, 1)], { Start = addr (0, 0); End = addr (10, 2) })
    case ([emptyCell (0, 0, 1, 1); emptyCell (10, 2, 2, 5)], { Start = addr (0, 0); End = addr (11, 6) })
    run test
  }

let ``moveDownでセル群をまとめて下に移動できる`` =
  let test (cells, n, expected) = test {
    do! assertEquals expected (Cells.moveDown n cells)
  }
  parameterize {
    case ([emptyCell (0, 0, 1, 1)], 1, ([emptyCell (1, 0, 1, 1)], { Start = addr (1, 0); End = addr (1, 0) }))
    case ([emptyCell (0, 0, 1, 1)], 2, ([emptyCell (2, 0, 1, 1)], { Start = addr (2, 0); End = addr (2, 0) }))
    case ([emptyCell (0, 0, 1, 1)
           emptyCell (1, 0, 2, 2)], 1, ([emptyCell (1, 0, 1, 1); emptyCell (2, 0, 2, 2)], { Start = addr (1, 0); End = addr (3, 1) }))
    run test
  }
