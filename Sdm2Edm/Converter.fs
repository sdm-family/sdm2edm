module Sdm2Edm.Converter

open Sdm
open Edm

let rec convertComponent (rule: ConvertionRule) (address: Address) = function
| Heading (groups, level, text) -> rule.Heading(address, groups, level, text)
| Paragraph (groups, lines) -> rule.Paragraph(address, groups, lines)
| List (groups, items) ->
    let startAddress = address
    let _, endAddress, cells =
      items
      |> Seq.fold (fun (address, _, cells) item ->
          let (address, res) = convertComponent rule address item
          let (address, res) = rule.ListItem(address, groups, res)
          let endCell = res |> List.last
          (address, { Address.Row = endCell.Row; Column = endCell.Column }, seq { yield! cells; yield! res })) (address, address, Seq.empty)
    rule.List(startAddress, endAddress, groups, cells |> Seq.toList)
| Table (groups, contents) -> (address, [])

let convertPage (rule: ConvertionRule) (page: Page) : Sheet =
  { Sheet.Name = page.Name
    Cells =
      page.Components
      |> Seq.fold (fun (address, cells) c ->
          let (address, res) = convertComponent rule address c
          (address, seq { yield! cells; yield! res })) (Address.A1, Seq.empty)
      |> snd
      |> Seq.toList }

let convert (rule: ConvertionRule) (pages: Page list) : Sheet list =
  pages |> List.map (convertPage rule)