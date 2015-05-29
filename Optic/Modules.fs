namespace Optic

module Choice =

  // right bias
  let map f = function
  | Choice1Of2 l -> Choice1Of2 l
  | Choice2Of2 r -> Choice2Of2 (f r)

  let bimap f g = function
  | Choice1Of2 l -> Choice1Of2 (f l)
  | Choice2Of2 r -> Choice2Of2 (g r)

module POptional =

  let inline modify f (p: POptional<_, _, _, _>) = p.Modify(f)
  let inline modifyChoiceF f (p: POptional<_, _, _, _>) = p.ModifyChoiceF(f)
  let inline modifyOptionF f (p: POptional<_, _, _, _>) = p.ModifyOptionF(f)
  let inline modifyListF f (p: POptional<_, _, _, _>) = p.ModifyListF(f)
  let inline modifyTrampolineF f (p: POptional<_, _, _, _>) = p.ModifyTrampolineF(f)
  let inline getOrModify s (p: POptional<_, _, _, _>) = p.GetOrModify(s)
  let inline set b (p: POptional<_, _, _, _>) = p.Set(b)
  let inline getOption s (p: POptional<_, _, _, _>) = p.GetOption(s)

  let modifyOption f p = fun s -> getOption s p |> Option.map f
  let setOption b p = modifyOption (fun _ -> b) p
  let isMatching s p = getOption s p |> Option.isSome

  let poptional getOrModify set = { new POptional<_, _, _, _> with
    member __.GetOrModify(s) = getOrModify s
    member __.Set(a) = set a
    member this.GetOption(s) =
      match this.GetOrModify(s) with
      | Choice1Of2 _ -> None
      | Choice2Of2 r -> Some r
    member this.ModifyFunctionF(f) = fun s ->
      match this.GetOrModify(s) with
      | Choice1Of2 l -> fun _ -> l
      | Choice2Of2 a -> (fun b -> this.Set(b) s) << f a
    member this.ModifyTrampolineF(f) = fun s ->
      match this.GetOrModify(s) with
      | Choice1Of2 l -> Trampoline.purely l
      | Choice2Of2 r -> f r |> Trampoline.map (fun b -> this.Set(b) s)
    member this.ModifyOptionF(f) = fun s ->
      match this.GetOrModify(s) with
      | Choice1Of2 l -> Some l
      | Choice2Of2 r -> f r |> Option.map (fun b -> this.Set(b) s)
    member this.ModifyListF(f) = fun s ->
      match this.GetOrModify(s) with
      | Choice1Of2 l ->  [l]
      | Choice2Of2 r -> f r |> List.map (fun b -> this.Set(b) s)
    member this.ModifyChoiceF(f) = fun s ->
      match this.GetOrModify(s) with
      | Choice1Of2 l -> Choice2Of2 l
      | Choice2Of2 t -> f t |> Choice.map (fun b -> this.Set(b) s)
    member this.Modify(f) = fun s ->
      match this.GetOrModify(s) with
      | Choice1Of2 l ->  l
      | Choice2Of2 r -> set (f r) s
  }

module Optional =

  let inline modify f (p: Optional<_, _>) = p.Modify(f)
  let inline modifyChoiceF f (p: Optional<_, _>) = p.ModifyChoiceF(f)
  let inline modifyOptionF f (p: Optional<_, _>) = p.ModifyOptionF(f)
  let inline modifyListF f (p: Optional<_, _>) = p.ModifyListF(f)
  let inline modifyTrampolineF f (p: Optional<_, _>) = p.ModifyTrampolineF(f)
  let inline getOrModify s (p: Optional<_, _>) = p.GetOrModify(s)
  let inline set b (p: Optional<_, _>) = p.Set(b)
  let inline getOption s (p: Optional<_, _>) = p.GetOption(s)

module Getter =

  let inline getter f = Getter.Getter(f)
  let inline get s (g: Getter<_, _>) = g.Get(s)
  let inline sum other (g: Getter<_, _>) = g.Sum(other)
  let inline product other (g: Getter<_, _>) = g.Product(other)
  let inline first (g: Getter<_, _>) = g.First()
  let inline second (g: Getter<_, _>) = g.Second()
  let inline asFold (g: Getter<_, _>) = g.AsFold()
  let inline composeFold other (g: Getter<_, _>) = g.ComposeFold(other)
  let inline composeGetter other (g: Getter<_, _>) = g.ComposeGetter(other)

module Fold =

  let inline foldMap m f (fold: Fold<_, _>) = fold.FoldMap(m, f)
  let inline fold m (f: Fold<_, _>) = f.Fold(m)
  let inline composeFold other (this: Fold<_, _>) = this.ComposeFold(other)

  let getAll s f = foldMap Monoid.list (fun x -> [x]) f s

  let find pred f = foldMap Monoid.option (fun a -> if pred a then Some a else None) f

  let headOption s f = find (fun _ -> true) f s

  let all pred f = foldMap Monoid.conjunction pred f

  let sum (this: Fold<_, _>) (other: Fold<_, _>) = { new Fold<Choice<_, _>, _>() with
    member __.FoldMap(m, f) = function
      | Choice1Of2 l -> this.FoldMap(m, f) l
      | Choice2Of2 r -> other.FoldMap(m, f) r
    }

  let codiagonal<'A> = { new Fold<Choice<'A, 'A>, 'A>() with
    member __.FoldMap(_, f) = function
      | Choice1Of2 l -> f l
      | Choice2Of2 r -> f r
    }
