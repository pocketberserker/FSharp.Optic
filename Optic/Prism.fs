namespace Optic

type Prism<'S, 'A> = {
  PPrism: PPrism<'S, 'S, 'A, 'A>
}
with
  interface PPrism<'S, 'S, 'A, 'A> with
    member this.GetOrModify(s) = this.PPrism.GetOrModify(s)
    member this.ReverseGet(a) = this.PPrism.ReverseGet(a)
    member this.GetOption(s) = this.PPrism.GetOption(s)
