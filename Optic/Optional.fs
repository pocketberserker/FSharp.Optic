namespace Optic

type Optional<'S, 'A> = {
  POptional: POptional<'S, 'S, 'A, 'A>
}
with
  interface POptional<'S, 'S, 'A, 'A> with
    member this.Set(a) = this.POptional.Set(a)
    member this.ModifyTrampolineF(f) = this.POptional.ModifyTrampolineF(f)
    member this.ModifyOptionF(f) = this.POptional.ModifyOptionF(f)
    member this.ModifyListF(f) = this.POptional.ModifyListF(f)
    member this.ModifyFunctionF(f) = this.POptional.ModifyFunctionF(f)
    member this.ModifyChoiceF(f) = this.POptional.ModifyChoiceF(f)
    member this.Modify(f) = this.POptional.Modify(f)
    member this.GetOrModify(s) = this.POptional.GetOrModify(s)
    member this.GetOption(s) = this.POptional.GetOption(s)
