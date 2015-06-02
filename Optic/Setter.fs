namespace Optic

type Setter<'S, 'A> = {
  PSetter: PSetter<'S, 'S, 'A, 'A>
}
with
  member inline this.Modify(f) = this.PSetter.Modify(f)
  member inline this.Set(a) = this.PSetter.Set(a)
  interface PSetter<'S, 'S, 'A, 'A> with
    member this.Modify(f) = this.Modify(f)
    member this.Set(a) = this.Set(a)
