namespace Optic

type PIso<'S, 'T, 'A, 'B> =
  abstract member Get: 'S -> 'A
  abstract member ReverseGet: 'B -> 'T
  abstract member Reverse: unit -> PIso<'B, 'A, 'T, 'S>
