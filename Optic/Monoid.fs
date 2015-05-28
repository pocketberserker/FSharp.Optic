namespace Optic

type Semigroup<'F> =
  abstract member Append: 'F * 'F -> 'F

type Monoid<'F> =
  inherit Semigroup<'F>
  abstract member Zero: unit -> 'F
