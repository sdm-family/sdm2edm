namespace NLNagoya

open Sdm
open System

type TitlePage =
  { MainTitle: string
    SubTitle: string
    Speaker: string
    Date: DateTime }

type ListItem =
  | TextListItem of string
  | Nested of string * ListItem list

type ListPage =
  { Heading: string
    Items: ListItem list }

type PresentationPage =
  | TitlePage of TitlePage
  | ListPage of ListPage

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PresentationPage =
  let private titlePageToSdm { MainTitle = mt; SubTitle = st; Speaker = s; Date = d } no =
    { Name = string no
      Components = [ Heading ([Styles.mainTitle], 1, Text.create [TextSegment.fromString mt])
                     Heading ([Styles.subTitle], 2, Text.create [TextSegment.fromString st])
                     Heading ([Styles.speaker], 3, Text.create [TextSegment.fromString s])
                     Heading ([Styles.date], 3, Text.create [TextSegment.fromString (d.ToString("yyyy年MM月dd日"))]) ] }

  let private listPageToSdm { Heading = h; Items = items } no =
    { Name = string no
      Components = [ Heading ([Styles.title], 1, Text.create [TextSegment.fromString h]) ] }

  let private toSdmImpl i = function
  | TitlePage titlePage -> titlePageToSdm titlePage i
  | ListPage listPage -> listPageToSdm listPage i

  let toSdm pages = pages |> List.mapi toSdmImpl