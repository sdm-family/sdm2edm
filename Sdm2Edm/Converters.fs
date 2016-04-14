namespace Sdm2Edm

open Sdm
open Edm

module Converters =
  module TextSegment =
    let toRichTextSegment (seg: TextSegment) =
      { RichTextSegment.Value = seg.Value; FontInfo = NoFontInfo }

  module RichTextSegment =
    let ofTextSegment (seg: TextSegment) =
      { RichTextSegment.Value = seg.Value; FontInfo = NoFontInfo }

  module Text =
    let toRichText (text: Text) =
      RichText.createWithoutFontInfo (text |> Text.map TextSegment.toRichTextSegment)

    let toOneRichTextCell (address: Address) (text: Text) =
      [ Cell.richText (address.Row, address.Column) (1, 1) (toRichText text) ]

  module RichText =
    let ofText (text: Text) =
      RichText.createWithoutFontInfo (text |> Text.map TextSegment.toRichTextSegment)

    let toOneCell (address: Address) richText =
      [ Cell.richText (address.Row, address.Column) (1, 1) richText ]
