module TableConverterTest

open Persimmon
open UseTestNameByReflection

open Sdm2Edm
open Edm

let (=>) (r, c) cell = (r, c, cell)

let ``空のTableConverterのインスタンスが生成できる`` = test {
  let converter = TableConverter([])
  do! assertEquals [] converter.Cells
  do! assertEquals 0 converter.Rows
  do! assertEquals 0 converter.Columns
}

let ``1つの要素を持つTableConverterのインスタンスが生成できる`` = test {
  let converter = TableConverter([(0, 0) => emptyCell (0, 0, 1, 1)])
  do! assertEquals [emptyCell (0, 0, 1, 1)] converter.Cells
  do! assertEquals 1 converter.Rows
  do! assertEquals 1 converter.Columns
}

let ``複数要素を持つTableConverterのインスタンスが生成できる`` = test {
  let converter =
    TableConverter([(0, 2) => emptyCell (0, 2, 1, 1)
                    (1, 0) => emptyCell (1, 0, 1, 1)
                    (0, 0) => emptyCell (0, 0, 1, 1)
                    (0, 1) => emptyCell (0, 1, 1, 1)])
  // 要素はアドレスでソートされる
  do! assertEquals [emptyCell (0, 0, 1, 1); emptyCell (0, 1, 1, 1); emptyCell (0, 2, 1, 1); emptyCell (1, 0, 1, 1)] converter.Cells
  do! assertEquals 2 converter.Rows
  do! assertEquals 3 converter.Columns
}

let ``AdjustRowAddressで指定行のRowにずらし済みの総行数を加算できる`` =
  let test (cells, adjusteds, row, expected) = test {
    let converter = TableConverter(cells)
    do 
      for col, adjustedRow in adjusteds do
        converter.SetAdjustedDataForTest(col, adjustedRow)
    converter.AdjustRowAddress(row)
    do! assertEquals expected (converter.Cells)
  }
  parameterize {
    case ([(0, 0) => emptyCell (0, 0, 1, 1)], [0, 0], 0, [emptyCell (0, 0, 1, 1)])
    case ([(0, 0) => emptyCell (0, 0, 1, 1)], [0, 1], 0, [emptyCell (1, 0, 1, 1)])
    case ([(0, 0) => emptyCell (0, 0, 2, 1)], [0, 5], 0, [emptyCell (5, 0, 2, 1)])
    run test
  }

let ``MaxRowAddressで指定行での最大行アドレスを算出できる`` =
  let empty row =
    (row, 0) => emptyCell (row, 0, 1, 1)
  let test (cells, row, expected) = test {
    let converter = TableConverter(cells)
    do! assertEquals expected (converter.MaxRowAddress(row))
  }
  parameterize {
    case ([(0, 0) => emptyCell (0, 0, 1, 1)], 0, 1)
    case ([(0, 10) => emptyCell (0, 10, 1, 1)], 0, 1)
    case ([(0, 0) => emptyCell (0, 0, 5, 1)], 0, 5)
    case ([empty 0; empty 1; empty 2; (3, 0) => emptyCell (3, 0, 1, 1)], 3, 4)
    case ([empty 0; empty 1; empty 2; (3, 10) => emptyCell (3, 10, 1, 1)], 3, 4)
    case ([empty 0; empty 1; empty 2; (3, 10) => emptyCell (3, 10, 5, 1)], 3, 8)
    run test
  }

let ``ExtendRowEndToUnifyで指定行の高さをそろえられる`` =
  let test (cells, adjusteds, row, expectedCells, expectedAdjustedRows) = test{
    let converter = TableConverter(cells)
    do 
      for col, adjustedRow in adjusteds do
        converter.SetAdjustedDataForTest(col, adjustedRow)
    converter.ExtendRowEndToUnify(row)
    do! assertEquals expectedCells converter.Cells
    do! assertEquals expectedAdjustedRows converter.GetAdjustedTableForTest
  }
  parameterize {
    case ([(0, 0) => emptyCell (0, 0, 1, 1)], [(0, 0)], 0, [emptyCell (0, 0, 1, 1)], [0])
    case ([(0, 0) => emptyCell (0, 0, 1, 1); (0, 1) => emptyCell (0, 1, 2, 1)], [(0, 0)], 0, [emptyCell (0, 0, 2, 1); emptyCell (0, 1, 2, 1)], [1; 0])
    case ([(0, 0) => emptyCell (0, 0, 1, 1); (1, 0) => emptyCell (1, 0, 1, 1)], [(0, 0)], 1, [emptyCell (0, 0, 1, 1); emptyCell (1, 0, 1, 1)], [0])
    case ([(0, 0) => emptyCell (0, 0, 1, 1)
           (1, 0) => emptyCell (1, 0, 1, 1)
           (1, 1) => emptyCell (1, 1, 5, 1)], [(0, 0)], 1,
          [emptyCell (0, 0, 1, 1)
           emptyCell (1, 0, 5, 1)
           emptyCell (1, 1, 5, 1)], [4; 0])
    run test
  }