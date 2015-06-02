namespace Optic

type Optional<'S, 'A> = {
  POptional: POptional<'S, 'S, 'A, 'A>
}
with
  member inline this.Set(a) = this.POptional.Set(a)
  member inline this.ModifyTrampolineF(f) = this.POptional.ModifyTrampolineF(f)
  member inline this.ModifyOptionF(f) = this.POptional.ModifyOptionF(f)
  member inline this.ModifyListF(f) = this.POptional.ModifyListF(f)
  member inline this.ModifyFunctionF(f) = this.POptional.ModifyFunctionF(f)
  member inline this.ModifyChoiceF(f) = this.POptional.ModifyChoiceF(f)
  member inline this.Modify(f) = this.POptional.Modify(f)
  member inline this.GetOrModify(s) = this.POptional.GetOrModify(s)
  member inline this.GetOption(s) = this.POptional.GetOption(s)
  interface POptional<'S, 'S, 'A, 'A> with
    member this.Set(a) = this.Set(a)
    member this.ModifyTrampolineF(f) = this.ModifyTrampolineF(f)
    member this.ModifyOptionF(f) = this.ModifyOptionF(f)
    member this.ModifyListF(f) = this.ModifyListF(f)
    member this.ModifyFunctionF(f) = this.ModifyFunctionF(f)
    member this.ModifyChoiceF(f) = this.ModifyChoiceF(f)
    member this.Modify(f) = this.Modify(f)
    member this.GetOrModify(s) = this.GetOrModify(s)
    member this.GetOption(s) = this.GetOption(s)
