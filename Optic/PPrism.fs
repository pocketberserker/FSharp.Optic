namespace Optic

type PPrism<'S, 'T, 'A, 'B> =
  abstract member GetOrModify: 'S -> Choice<'T, 'A>
  abstract member ReverseGet: 'B -> 'T
  abstract member GetOption: 'S -> 'A option
