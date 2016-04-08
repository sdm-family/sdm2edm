namespace NLNagoya

open Sdm

module Styles =
  // heading styles
  let mainTitle = TextStyleGroup("MainTitle")
  let subTitle = TextStyleGroup("SubTitle")
  let speaker = TextStyleGroup("Speaker")
  let date = TextStyleGroup("Date")

  let title = TextStyleGroup("Title")

  // paragraph styles
  let image path = TextStyleGroup("Image" + path)

  // list styles
  let unorderedList = ListStyleGroup("UnorderedList")
  let listItem = TextStyleGroup("ListItem")