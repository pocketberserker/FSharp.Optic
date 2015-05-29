namespace Optic

type Traversal<'S, 'A> = {
  PTraversal: PTraversal<'S, 'S, 'A, 'A>
}
with
  interface PTraversal<'S, 'S, 'A, 'A> with
    member this.ModifyTrampolineF(f) = this.PTraversal.ModifyTrampolineF(f)
    member this.ModifyOptionF(f) = this.PTraversal.ModifyOptionF(f)
    member this.ModifyListF(f) = this.PTraversal.ModifyListF(f)
    member this.ModifyFunctionF(f) = this.PTraversal.ModifyFunctionF(f)
    member this.ModifyChoiceF(f) = this.PTraversal.ModifyChoiceF(f)
    member this.FoldMap(m, f) = this.PTraversal.FoldMap(m, f)
