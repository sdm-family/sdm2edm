﻿namespace NLNagoya

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

type ImageAndListPage =
  { Heading: string
    ImagePath: string
    Items: ListItem list }

type SectionPage =
  { SectionName: string }

type ShapePage =
  { Heading: string
    ShapeDescription: string }

type CodePage =
  { Heading: string
    Code: string }

type PresentationPage =
  | TitlePage of TitlePage
  | SectionPage of SectionPage
  | ListPage of ListPage
  | ImageAndListPage of ImageAndListPage
  | ShapePage of ShapePage
  | CodePage of CodePage

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PresentationPage =
  let private titlePageToSdm { MainTitle = mt; SubTitle = st; Speaker = s; Date = d } no =
    { Name = string no
      Components = [ Heading ([Styles.mainTitle], 1, Text.create [TextSegment.fromString mt])
                     Heading ([Styles.subTitle], 2, Text.create [TextSegment.fromString st])
                     Heading ([Styles.speaker], 3, Text.create [TextSegment.fromString s])
                     Heading ([Styles.date], 3, Text.create [TextSegment.fromString (d.ToString("yyyy年MM月dd日"))]) ] }

  let private sectionPageToSdm { SectionName = n } no =
    { Name = string no
      Components = [ Heading ([Styles.sectionName], 1, Text.create [TextSegment.fromString n]) ] }

  let rec private listItemToSdm = function
  | TextListItem item -> [Paragraph ([Styles.listItem], [Text.create [TextSegment.fromString item]])]
  | Nested (item, children) -> [ Paragraph ([Styles.listItem], [Text.create [TextSegment.fromString item]])
                                 List ([Styles.unorderedList], children |> List.map (listItemToSdm)) ]

  let private listPageToSdm { ListPage.Heading = h; Items = items } no =
    { Name = string no
      Components = [ Heading ([Styles.title], 1, Text.create [TextSegment.fromString h])
                     List ([Styles.unorderedList], items |> List.map listItemToSdm) ] }

  let private imageAndListPageToSdm { ImageAndListPage.Heading = h; ImagePath = p; Items = items } no =
    { Name = string no
      Components = [ Heading ([Styles.title], 1, Text.create [TextSegment.fromString h])
                     Paragraph ([Styles.image p], [])
                     List ([Styles.unorderedList], items |> List.map listItemToSdm) ] }

  let private shapePageToSdm { ShapePage.Heading = h; ShapeDescription = sd } no =
    { Name = string no
      Components = [ Heading ([Styles.title], 1, Text.create [TextSegment.fromString h])
                     Paragraph ([Styles.shape sd], []) ] }

  let private codePageToSdm { CodePage.Heading = h; Code = c } no =
    { Name = string no
      Components = [ Heading ([Styles.title], 1, Text.create [TextSegment.fromString h])
                     Paragraph ([Styles.fsharp], [ Text.create [TextSegment.fromString c] ]) ] }

  let private toSdmImpl i = function
  | TitlePage titlePage -> titlePageToSdm titlePage i
  | SectionPage sectionPage -> sectionPageToSdm sectionPage i
  | ListPage listPage -> listPageToSdm listPage i
  | ImageAndListPage imgAndListPage -> imageAndListPageToSdm imgAndListPage i
  | ShapePage shapePage -> shapePageToSdm shapePage i
  | CodePage codePage -> codePageToSdm codePage i

  let toSdm pages = pages |> List.mapi toSdmImpl