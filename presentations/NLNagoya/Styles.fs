namespace NLNagoya

open Sdm

module Styles =
  // heading styles
  let mainTitle = TextStyleGroup("MainTitle")
  let subTitle = TextStyleGroup("SubTitle")
  let speaker = TextStyleGroup("Speaker")
  let date = TextStyleGroup("Date")

  let title = TextStyleGroup("Title")

  let private check x xs = if xs |> List.contains x then Some () else None

  module Heading =
    let (|MainTitle|_|) groups = check mainTitle groups
    let (|SubTitle|_|) groups = check subTitle groups
    let (|Speaker|_|) groups = check speaker groups
    let (|Date|_|) groups = check date groups
    let (|Title|_|) groups = check title groups
