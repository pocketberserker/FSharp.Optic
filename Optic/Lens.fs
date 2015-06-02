namespace Optic

type Lens<'S, 'A> = {
  PLens: PLens<'S, 'S, 'A, 'A>
}
with
  member this.Get(s) = this.PLens.Get(s)
  member this.Set(a) = this.PLens.Set(a)
  member this.ModifyTrampolineF(f) = this.PLens.ModifyTrampolineF(f)
  member this.ModifyOptionF(f) = this.PLens.ModifyOptionF(f)
  member this.ModifyListF(f) = this.PLens.ModifyListF(f)
  member this.ModifyFunctionF(f) = this.PLens.ModifyFunctionF(f)
  member this.ModifyChoiceF(f) = this.PLens.ModifyChoiceF(f)
  member this.Modify(f) = this.PLens.Modify(f)
  interface PLens<'S, 'S, 'A, 'A> with
    member this.Get(s) = this.Get(s)
    member this.Set(a) = this.Set(a)
    member this.ModifyTrampolineF(f) = this.ModifyTrampolineF(f)
    member this.ModifyOptionF(f) = this.ModifyOptionF(f)
    member this.ModifyListF(f) = this.ModifyListF(f)
    member this.ModifyFunctionF(f) = this.ModifyFunctionF(f)
    member this.ModifyChoiceF(f) = this.ModifyChoiceF(f)
    member this.Modify(f) = this.Modify(f)
