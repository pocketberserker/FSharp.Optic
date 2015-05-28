namespace Optic

[<AbstractClass>]
type Fold<'S, 'A> () =
  abstract member FoldMap: #Monoid<'M> * ('A -> 'M) -> ('S -> 'M)
  member this.Fold(m: #Monoid<'A>): ('S -> 'A) =
    this.FoldMap(m, id)
  member this.ComposeFold(other: Fold<_, _>) = { new Fold<_, _>() with
    member __.FoldMap(m, f) = this.FoldMap(m, other.FoldMap(m, f)) }
