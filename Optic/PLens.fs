namespace Optic

type PLens<'S, 'T, 'A, 'B> =
  abstract member Get: 'S -> 'A
  abstract member Set: 'B -> ('S -> 'T)
  abstract member ModifyFunctionF: ('A -> ('C -> 'B)) -> ('S -> ('C -> 'T))
  abstract member ModifyChoiceF: ('A -> Choice<'L, 'B>) -> ('S -> Choice<'L, 'T>)
  abstract member ModifyTrampolineF: ('A -> Trampoline<'B>) -> ('S -> Trampoline<'T>)
  abstract member ModifyListF: ('A -> 'B list) -> ('S -> 'T list)
  abstract member ModifyOptionF: ('A -> 'B option) -> ('S -> 'T option)
  abstract member Modify: ('A -> 'B) -> ('S -> 'T)
