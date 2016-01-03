namespace Sdm2Edm

open Edm

type PageHeader = {
  Cells: Cell list
}

type PagerInfo = {
  MaxRows: int
  MaxColumns: int
  PageHeader: PageHeader
}
