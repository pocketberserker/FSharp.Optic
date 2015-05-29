namespace Optic

type PSetter<'S, 'T, 'A, 'B> =
  abstract member Modify: ('A -> 'B) -> ('S -> 'T)
  abstract member Set: 'B -> ('S -> 'T)
