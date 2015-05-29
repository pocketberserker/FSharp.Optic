namespace Optic

type Setter<'S, 'A> = {
  PSetter: PSetter<'S, 'S, 'A, 'A>
}
with
  interface PSetter<'S, 'S, 'A, 'A> with
    member this.Modify(f) = this.PSetter.Modify(f)
    member this.Set(a) = this.PSetter.Set(a)
