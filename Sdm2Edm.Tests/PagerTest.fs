module PagerTest

open Persimmon
open UseTestNameByReflection

open Sdm2Edm
open Edm

let ``ページがあふれたら改シートするPagerが使える`` =
  let header =
    [
      { emptyCell (0, 0, 1, 10) with Data = Other "ヘッダー" }
      { emptyCell (1, 0, 1, 5) with Data = Other "page" }
      { emptyCell (1, 5, 1, 5) with Data = Other PageNumber }
    ]
  let createHeader page =
    header
    |> List.map (fun cell -> { cell with Data = match cell.Data with Other (:? Placeholder) -> Other page | _ -> cell.Data })
  let pager = Pager.breakSheetPager { MaxRows = 4; MaxColumns = 10; PageHeader = { Cells = header } }
  let test (sheets, expected) = test {
    do! assertEquals expected (pager.Page(sheets))
  }
  parameterize {
    case ([], [])
    case ([ { Name = "1"; Cells = []; Drawings = [] } ], [ { Name = "1"; Cells = createHeader 1; Drawings = [] } ])
    case ([ { Name = "1"; Cells = [ emptyCell (0, 0, 1, 1) ]; Drawings = [] } ],
          [ { Name = "1"; Cells = [ yield! createHeader 1; yield emptyCell (2, 0, 1, 1) ]; Drawings = [] } ])
    case ([ { Name = "1"; Cells = [ emptyCell (0, 0, 1, 1); emptyCell (1, 0, 1, 1) ]; Drawings = [] } ],
          [ { Name = "1"; Cells = [ yield! createHeader 1
                                    yield emptyCell (2, 0, 1, 1)
                                    yield emptyCell (3, 0, 1, 1) ]; Drawings = [] } ])

    case ([ { Name = "1"; Cells = [ emptyCell (0, 0, 1, 1); emptyCell (1, 0, 1, 1)
                                    emptyCell (2, 0, 2, 1) ]; Drawings = [] } ],
          [ { Name = "1"; Cells = [ yield! createHeader 1
                                    yield emptyCell (2, 0, 1, 1)
                                    yield emptyCell (3, 0, 1, 1) ]; Drawings = [] }
            { Name = "1(2)"; Cells = [ yield! createHeader 2
                                       yield emptyCell (2, 0, 2, 1) ]; Drawings = [] }])

    case ([ { Name = "1"; Cells = [ emptyCell (0, 0, 1, 1); emptyCell (1, 0, 1, 1)
                                    emptyCell (2, 0, 2, 1); emptyCell (3, 0, 1, 1) ]; Drawings = [] } ],
          [ { Name = "1"; Cells = [ yield! createHeader 1
                                    yield emptyCell (2, 0, 1, 1)
                                    yield emptyCell (3, 0, 1, 1) ]; Drawings = [] }
            { Name = "1(2)"; Cells = [ yield! createHeader 2
                                       yield emptyCell (2, 0, 2, 1)
                                       yield emptyCell (3, 0, 1, 1) ]; Drawings = [] }])

    case ([ { Name = "1"; Cells = [ emptyCell (0, 0, 1, 1); emptyCell (1, 0, 1, 1)
                                    emptyCell (2, 0, 2, 1); emptyCell (3, 0, 1, 1)
                                    emptyCell (4, 0, 1, 2) ]; Drawings = [] } ],
          [ { Name = "1"; Cells = [ yield! createHeader 1
                                    yield emptyCell (2, 0, 1, 1)
                                    yield emptyCell (3, 0, 1, 1) ]; Drawings = [] }
            { Name = "1(2)"; Cells = [ yield! createHeader 2
                                       yield emptyCell (2, 0, 2, 1)
                                       yield emptyCell (3, 0, 1, 1) ]; Drawings = [] }
            { Name = "1(3)"; Cells = [ yield! createHeader 3
                                       yield emptyCell (2, 0, 1, 2) ]; Drawings = [] }])

    case ([ { Name = "1"; Cells = [ emptyCell (0, 0, 1, 1)
                                    emptyCell (1, 0, 2, 1)
                                    emptyCell (3, 0, 1, 1) ]; Drawings = [] } ],
          [ { Name = "1"; Cells = [ yield! createHeader 1
                                    yield emptyCell (2, 0, 1, 1) ]; Drawings = [] }
            { Name = "1(2)"; Cells = [ yield! createHeader 2
                                       yield emptyCell (2, 0, 2, 1) ]; Drawings = [] }
            { Name = "1(3)"; Cells = [ yield! createHeader 3
                                       yield emptyCell (2, 0, 1, 1) ]; Drawings = [] } ])
    run test
  }