namespace Optic

type Iso<'S, 'A> = {
  PIso: PIso<'S, 'S, 'A, 'A>
}
with
  interface PIso<'S, 'S, 'A, 'A> with
    member this.Get(s) = this.PIso.Get(s)
    member this.ReverseGet(a) = this.PIso.ReverseGet(a)
    member this.Reverse() = this.PIso.Reverse()
