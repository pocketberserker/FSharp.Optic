namespace Optic

type Semigroup<'F> =
  abstract member Append: 'F * 'F -> 'F

type Monoid<'F> =
  inherit Semigroup<'F>
  abstract member Zero: unit -> 'F

module Monoid =

  let list = { new Monoid<_> with
    member __.Append(a, b) = List.append a b
    member __.Zero() = [] }

  let option = { new Monoid<_> with
    member __.Append(a, b) = if Option.isSome a then a else b
    member __.Zero() = None }

  let conjunction = { new Monoid<_> with
    member __.Append(a, b) = a && b
    member __.Zero() = true }
