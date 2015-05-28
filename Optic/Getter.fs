namespace Optic

[<AbstractClass>]
type Getter<'S, 'A> () =
  abstract member Get: 'S -> 'A
  static member Getter<'S, 'A>(f) = { new Getter<'S, 'A>() with
    member __.Get(s) = f s }
  member this.AsFold() = { new Fold<_, _>() with
    member __.FoldMap(m, f) = fun s -> f (this.Get(s)) }
  member this.Sum(other: Getter<_, _>) =
    Getter.Getter(function | Choice1Of2 l -> this.Get(l) | Choice2Of2 r -> other.Get(r) )
  member this.Product(other: Getter<_, _>) =
    Getter.Getter(fun (p1, p2) -> (this.Get(p1), other.Get(p2)))
  member this.First() = Getter.Getter(fun (p1, p2) -> (this.Get(p1), p2))
  member this.Second() = Getter.Getter(fun (p1, p2) -> (p1, this.Get(p2)))
  member this.ComposeFold(other: Fold<'A, _>) = this.AsFold().ComposeFold(other)
  member this.ComposeGetter(other: Getter<_, _>) = Getter.Getter(fun s -> other.Get(this.Get(s)))
