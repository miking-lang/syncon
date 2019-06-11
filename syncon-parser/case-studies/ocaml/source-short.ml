[@@@foo]

let (x[@foo]) : unit [@foo] = ()[@foo]
  [@@foo]

type t =
  | Foo of (t[@foo]) [@foo]
[@@foo]

[@@@foo]
