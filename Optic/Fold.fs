namespace Optic

[<AbstractClass>]
type Fold<'S, 'A> =
  abstract member FoldMap: #Monoid<'M> * ('A -> 'M) -> ('S -> 'M)
  member this.Fold(m: #Monoid<'A>): ('S -> 'A) =
    this.FoldMap(m, id)
