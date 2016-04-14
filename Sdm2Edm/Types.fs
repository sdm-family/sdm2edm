namespace Sdm2Edm

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

  let nextColAddress range =
    { Row = range.Start.Row; Column = range.End.Column + 1 }

  let nextComponentStart range =
    let address = nextRowAddress range
    { Start = address; End = address }

  let nextColumnStart range =
    let address = nextColAddress range
    { Start = address; End = address }

