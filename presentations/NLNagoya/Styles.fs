namespace NLNagoya

open Sdm

module Styles =
  // heading styles
  let mainTitle = TextStyleGroup("MainTitle")
  let subTitle = TextStyleGroup("SubTitle")
  let speaker = TextStyleGroup("Speaker")
  let date = TextStyleGroup("Date")

  let title = TextStyleGroup("Title")

  let sectionName = TextStyleGroup("SectionName")

  // paragraph styles
  let image path = TextStyleGroup("Image" + path)

  // list styles
  let unorderedList = ListStyleGroup("UnorderedList")
  let listItem = TextStyleGroup("ListItem")

  // shape styles
  let shape description = TextStyleGroup("Shape" + description)