namespace Optic

[<AutoOpen>]
module internal Prelude =

  let inline konst a _ = a
  let inline flip f a b = f b a

module Function =

  let bind f g = fun m -> f (g m) m

  let apply2 cab ca = bind (fun f -> ca >> f) cab

module List =

  let inline singleton a = [a]

  let apply a l = a |> List.collect (flip List.map l)

module internal Option =

  let getOrElse defaultValue = function
  | Some v -> v
  | None -> defaultValue

  let apply fo o = fo |> Option.bind (fun f -> o |> Option.map f)

module internal Choice =

  let leftMap f = function
  | Choice1Of2 l -> Choice1Of2 (f l)
  | Choice2Of2 r -> Choice2Of2 r

  let rightMap f = function
  | Choice1Of2 l -> Choice1Of2 l
  | Choice2Of2 r -> Choice2Of2 (f r)

  let bimap f g = function
  | Choice1Of2 l -> Choice1Of2 (f l)
  | Choice2Of2 r -> Choice2Of2 (g r)

  let rightBind f = function
  | Choice1Of2 l -> Choice1Of2 l
  | Choice2Of2 r -> f r

  let fold f g = function
  | Choice1Of2 l -> f l
  | Choice2Of2 r -> g r

  let rightApply a e = a |> rightBind (flip rightMap e)

  let rightToOption = function
  | Choice1Of2 _ -> None
  | Choice2Of2 r -> Some r

module internal InternalPIso =

  let piso get reverseGet = { new PIso<_, _, _, _> with
    member __.Get(s) = get s
    member __.ReverseGet(b) = reverseGet b
    member this.Reverse() = { new PIso<_, _, _, _> with
      member __.Get(s) = reverseGet s
      member __.ReverseGet(b) = get b
      member __.Reverse() = this
    }
  }

  let pid<'S, 'T> = { new PIso<'S, 'T, 'S, 'T> with
    member __.Get(s) = s
    member __.ReverseGet(t) = t
    member this.Reverse() = { new PIso<_, _, _, _> with
      member __.Get(s) = s
      member __.ReverseGet(t) = t
      member __.Reverse() = this
    }
  }

  let inline get s (i: PIso<_, _, _, _>) = i.Get(s)
  let inline reverseGet b (i: PIso<_, _, _, _>) = i.ReverseGet(b)

  let modifyFunctionF f i = fun s -> flip reverseGet i << (get s i |> f)
  let modifyChoiceF f i = fun s -> get s i |> f |> Choice.rightMap (flip reverseGet i)
  let modifyTrampolineF f i = fun s -> get s i |> f |> Trampoline.map (flip reverseGet i)
  let modifyOptionF f i = fun s -> get s i |> f |> Option.map (flip reverseGet i)
  let modifyListF f i = fun s -> get s i |> f |> List.map (flip reverseGet i)
  let modify f i = fun s -> get s i |> f |>flip reverseGet i
  let set b i = konst (reverseGet b i)

  let toFold p = { new Fold<_, _>() with
    member __.FoldMap(_, f) = fun s -> get s p |> f }

  let toGetter p = { new Getter<_, _>() with
    member __.Get(s) = get s p }

  let toSetter p = { new PSetter<_, _, _, _> with
    member __.Modify(f) = modify f p
    member __.Set(b) = set b p }

  let toTraversal p = { new PTraversal<_, _, _, _> with
    member __.ModifyFunctionF(f) = modifyFunctionF f p
    member __.ModifyTrampolineF(f) = modifyTrampolineF f p
    member __.ModifyOptionF(f) = modifyOptionF f p
    member __.ModifyListF(f) = modifyListF f p
    member __.ModifyChoiceF(f) = modifyChoiceF f p
    member __.Modify(f) = modify f p
    member __.FoldMap(_, f) = fun s -> get s p |> f
  }

  let toOptional p = { new POptional<_, _, _, _> with
    member __.Set(b) = set b p
    member __.GetOrModify(s) = Choice2Of2 (get s p)
    member __.GetOption(s) = Some (get s p)
    member __.ModifyFunctionF(f) = modifyFunctionF f p
    member __.ModifyTrampolineF(f) = modifyTrampolineF f p
    member __.ModifyOptionF(f) = modifyOptionF f p
    member __.ModifyListF(f) = modifyListF f p
    member __.ModifyChoiceF(f) = modifyChoiceF f p
    member __.Modify(f) = modify f p
  }

  let toPrism p = { new PPrism<_, _, _, _> with
    member __.GetOrModify(s) = Choice2Of2 (get s p)
    member __.ReverseGet(b) = reverseGet b p
    member __.GetOption(s) = Some (get s p)
  }

  let toLens p = { new PLens<_, _, _, _> with
    member __.Get(s) = get s p
    member __.Set(b) = set b p
    member __.ModifyFunctionF(f) = modifyFunctionF f p
    member __.ModifyTrampolineF(f) = modifyTrampolineF f p
    member __.ModifyOptionF(f) = modifyOptionF f p
    member __.ModifyListF(f) = modifyListF f p
    member __.ModifyChoiceF(f) = modifyChoiceF f p
    member __.Modify(f) = modify f p
  }

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

module PSetter =

  let inline modify f (s: PSetter<_, _, _, _>) = s.Modify(f)
  let inline set b (s: PSetter<_, _, _, _>) = s.Set(b)

  let psetter modify = { new PSetter<_, _, _, _> with
    member __.Modify(f) = modify f
    member __.Set(b) = modify (konst b) }

  let sum this other = psetter (fun f e -> e |> Choice.bimap (modify f this) (modify f other))

  let codiagonal<'S, 'T> = psetter (fun (f: 'S -> 'T) e -> e |> Choice.bimap f f)

  let composeSetter this other = { new PSetter<_, _, _, _> with
    member __.Modify(f) = this |> modify (other |> modify f)
    member __.Set(b) = modify (set b other) this }

module Setter =

  let inline modify f (s: Setter<_, _>) = s.Modify(f)
  let inline set b (s: Setter<_, _>) = s.Set(b)

  let setter modify = { PSetter = PSetter.psetter modify }
  let sum this other = { PSetter = PSetter.sum this other }

  let codiagonal<'S> = { PSetter = PSetter.codiagonal<'S, 'S> }

  let composeSetter this other = { PSetter = PSetter.composeSetter this.PSetter other.PSetter }

module Getter =

  let inline getter f = Getter.Getter(f)
  let inline get s (g: Getter<_, _>) = g.Get(s)
  let inline sum other (g: Getter<_, _>) = g.Sum(other)
  let inline product other (g: Getter<_, _>) = g.Product(other)
  let inline fst (g: Getter<_, _>) = g.First()
  let inline snd (g: Getter<_, _>) = g.Second()
  let inline toFold (g: Getter<_, _>) = g.ToFold()
  let inline composeFold other (g: Getter<_, _>) = g.ComposeFold(other)
  let inline composeGetter other (g: Getter<_, _>) = g.ComposeGetter(other)
  let codiagonal<'A> = getter (function | Choice1Of2 (a: 'A) -> a | Choice2Of2 a -> a)

module PTraversal =

  let inline modifyTrampolineF f (t: PTraversal<_, _, _, _>) = t.ModifyTrampolineF(f)
  let inline modifyFunctionF f (t: PTraversal<_, _, _, _>) = t.ModifyFunctionF(f)
  let inline modifyListF f (t: PTraversal<_, _, _, _>) = t.ModifyListF(f)
  let inline modifyChoiceF f (t: PTraversal<_, _, _, _>) = t.ModifyChoiceF(f)
  let inline modifyOptionF f (t: PTraversal<_, _, _, _>) = t.ModifyOptionF(f)
  let inline foldMap m f (t: PTraversal<_, _, _, _>) = t.FoldMap(m, f)
  let inline fold m t = foldMap m id t
  let getAll s t = foldMap Monoid.list List.singleton t s
  let find pred t = foldMap Monoid.option (fun a -> if pred a then Some a else None) t
  let headOption s t = find (konst true) t s
  let exists pred t = foldMap Monoid.disjunction pred t
  let forall pred t = foldMap Monoid.conjunction pred t
  let inline modify f (t: PTraversal<_, _, _, _>) = t.Modify(f)
  let set b t = modify (konst b) t

  let sum this other = { new PTraversal<_, _, _, _> with
    member __.FoldMap(m, f) = Choice.fold (foldMap m f this) (foldMap m f other)
    member __.Modify(f) =
      Choice.fold (fun s -> modify f this s |> Choice1Of2)
        (fun s1 -> modify f other s1 |> Choice2Of2)
    member __.ModifyChoiceF(f) =
      Choice.fold (fun s -> modifyChoiceF f this s |> Choice.rightMap Choice1Of2)
        (fun s1 -> modifyChoiceF f other s1 |> Choice.rightMap Choice2Of2)
    member __.ModifyFunctionF(f) =
      Choice.fold (fun s -> modifyFunctionF f this s >> Choice1Of2)
        (fun s1 -> modifyFunctionF f other s1 >> Choice2Of2)
    member __.ModifyListF(f) =
      Choice.fold (fun s -> modifyListF f this s |> List.map Choice1Of2)
        (fun s1 -> modifyListF f other s1 |> List.map Choice2Of2)
    member __.ModifyOptionF(f) =
      Choice.fold (fun s -> modifyOptionF f this s |> Option.map Choice1Of2)
        (fun s1 -> modifyOptionF f other s1 |> Option.map Choice2Of2)
    member __.ModifyTrampolineF(f) =
      Choice.fold (fun s -> modifyTrampolineF f this s |> Trampoline.map Choice1Of2)
        (fun s1 -> modifyTrampolineF f other s1 |> Trampoline.map Choice2Of2)
  }

  let toFold this = { new Fold<_, _>() with
    member __.FoldMap(m, f) = foldMap m f this }

  let codiagonal = { new PTraversal<_, _, _, _> with
    member __.FoldMap(_, f) = Choice.fold f f
    member __.Modify(f) = Choice.bimap f f >> Choice.fold Choice1Of2 Choice2Of2
    member __.ModifyChoiceF(f) =
      Choice.bimap f f >> Choice.fold (Choice.rightMap Choice1Of2) (Choice.rightMap Choice2Of2)
    member __.ModifyFunctionF(f) =
      Choice.bimap f f >> Choice.fold (fun g -> g >> Choice1Of2) (fun g -> g >> Choice2Of2)
    member __.ModifyListF(f) =
      Choice.bimap f f >> Choice.fold (List.map Choice1Of2) (List.map Choice2Of2)
    member __.ModifyOptionF(f) =
      Choice.bimap f f >> Choice.fold (Option.map Choice1Of2) (Option.map Choice2Of2)
    member __.ModifyTrampolineF(f) =
      Choice.bimap f f >> Choice.fold (Trampoline.map Choice1Of2) (Trampoline.map Choice2Of2)
  }

  let ptraversal get1 get2 set = { new PTraversal<_, _, _, _> with
    member __.FoldMap(m, f) = fun s -> m.Append(get1 s |> f, get2 s |> f)
    member __.Modify(f) = fun s ->
      get2 s
      |> f
      |> (get1 s |> f |> fun b1 b2 -> set b1 b2 s)
    member __.ModifyChoiceF(f) = fun s ->
      get2 s
      |> f
      |> Choice.rightApply (get1 s |> f |> Choice.rightMap (fun b1 b2 -> set b1 b2 s))
    member __.ModifyFunctionF(f) = fun s ->
      Function.apply2 (f (get1 s) >> fun b1 b2 -> set b1 b2 s) (f (get2 s))
    member __.ModifyListF(f) = fun s ->
      get2 s
      |> f
      |> List.apply (get1 s |> f |> List.map (fun b1 b2 -> set b1 b2 s))
    member __.ModifyOptionF(f) = fun s ->
      get2 s
      |> f
      |> Option.apply (get1 s |> f |> Option.map (fun b1 b2 -> set b1 b2 s))
    member __.ModifyTrampolineF(f) = fun s ->
      get2 s
      |> f
      |> Trampoline.apply (get1 s |> f |> Trampoline.map (fun b1 b2 -> set b1 b2 s))
  }

  let toSetter t = PSetter.psetter (fun f -> modify f t)

  let composeFold this other = toFold this |> Fold.composeFold other
  let composeSetter this other = PSetter.composeSetter (toSetter this) other

  let composeTraversal this other = { new PTraversal<_, _, _, _> with
    member __.FoldMap(m, f) = foldMap m (foldMap m f other) this
    member __.Modify(f) = modify (modify f other) this
    member __.ModifyChoiceF(f) = modifyChoiceF (modifyChoiceF f other) this
    member __.ModifyFunctionF(f) = modifyFunctionF (modifyFunctionF f other) this
    member __.ModifyListF(f) = modifyListF (modifyListF f other) this
    member __.ModifyOptionF(f) = modifyOptionF (modifyOptionF f other) this
    member __.ModifyTrampolineF(f) = modifyTrampolineF (modifyTrampolineF f other) this
  }

  let composeIso this other = composeTraversal (InternalPIso.toTraversal other) this

module Traversal =

  let inline modifyFunctionF f (o: Traversal<_, _>) = o.ModifyFunctionF(f)
  let inline modifyOptionF f (o: Traversal<_, _>) = o.ModifyOptionF(f)
  let inline modifyListF f (o: Traversal<_, _>) = o.ModifyListF(f)
  let inline modifyTrampolineF f (o: Traversal<_, _>) = o.ModifyTrampolineF(f)
  let inline modifyChoiceF f (o: Traversal<_, _>) = o.ModifyChoiceF(f)
  let inline foldMap m f (o: Traversal<_, _>) = o.FoldMap(m, f)
  let inline sum a b = { PTraversal = PTraversal.sum a b }
  let inline toSetter o = { PSetter = PTraversal.toSetter o.PTraversal }
  let codiagonal = { PTraversal = PTraversal.codiagonal }
  let inline traversal get1 get2 set =
    { PTraversal = PTraversal.ptraversal get1 get2 (fun a1 a2 _ -> set a1 a2) }

  let composeSetter this other = { PSetter = PTraversal.composeSetter this.PTraversal other.PSetter }
  let composeTraversal this other = { PTraversal = PTraversal.composeTraversal this.PTraversal other.PTraversal }

module POptional =

  let inline modify f (p: POptional<_, _, _, _>) = p.Modify(f)
  let inline modifyChoiceF f (p: POptional<_, _, _, _>) = p.ModifyChoiceF(f)
  let inline modifyFunctionF f (p: POptional<_, _, _, _>) = p.ModifyFunctionF(f)
  let inline modifyOptionF f (p: POptional<_, _, _, _>) = p.ModifyOptionF(f)
  let inline modifyListF f (p: POptional<_, _, _, _>) = p.ModifyListF(f)
  let inline modifyTrampolineF f (p: POptional<_, _, _, _>) = p.ModifyTrampolineF(f)
  let inline getOrModify s (p: POptional<_, _, _, _>) = p.GetOrModify(s)
  let inline set b (p: POptional<_, _, _, _>) = p.Set(b)
  let inline getOption s (p: POptional<_, _, _, _>) = p.GetOption(s)

  let modifyOption f p = fun s -> getOption s p |> Option.map f
  let setOption b p = modifyOption (konst b) p
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
      | Choice1Of2 l -> konst l
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
      | Choice2Of2 t -> f t |> Choice.rightMap (fun b -> this.Set(b) s)
    member this.Modify(f) = fun s ->
      this.GetOrModify(s) |> Choice.fold id (f >> (flip set s))
  }

  let sum this other =
    let f = function
    | Choice1Of2 s -> getOrModify s this |> Choice.leftMap Choice1Of2
    | Choice2Of2 s1 -> other |> getOrModify s1 |> Choice.leftMap Choice2Of2
    poptional f (fun b e -> e |> Choice.bimap (set b this) (set b other))

  let fst this =
    let f (sc1, sc2) =
      getOrModify sc1 this
      |> Choice.bimap (fun t -> (t, sc2)) (fun a -> (a, sc2))
    poptional f (fun (bc1, bc2) (s1, _) -> (set bc1 this s1, bc2))

  let snd this =
    let f (cs1, cs2) =
      getOrModify cs2 this
      |> Choice.bimap (fun t -> (cs1, t)) (fun a -> (cs1, a))
    poptional f (fun (cb1, cb2) (_, s2) -> (cb1, set cb2 this s2))

  let toFold this = { new Fold<_, _>() with
    member __.FoldMap(m, f) = fun s ->
      this |> getOption s |> Option.map f |> Option.getOrElse (Monoid.zero m)
  }

  let toSetter this = { new PSetter<_, _, _, _> with
    member __.Modify(f) = modify f this
    member __.Set(b) = set b this }

  let toTraversal this = { new PTraversal<_, _, _, _> with
    member __.FoldMap(m, f) = fun s ->
      getOption s this |> Option.map f |> Option.getOrElse (Monoid.zero m)
    member __.ModifyChoiceF(c) = modifyChoiceF c this
    member __.ModifyFunctionF(f) = modifyFunctionF f this
    member __.ModifyListF(l) = modifyListF l this
    member __.ModifyOptionF(o) = modifyOptionF o this
    member __.ModifyTrampolineF(t) = modifyTrampolineF t this
    member __.Modify(t) = modify t this
  }

  let composeSetter this other = toSetter this |> PSetter.composeSetter other

  let composeOptional this other = { new POptional<_, _, _, _> with
    member __.GetOrModify(s) =
      getOrModify s this
      |> Choice.rightBind (fun a ->
        getOrModify a other
        |> Choice.bimap (fun b -> set b this s) id)
    member __.Set(a) = modify (set a other) this
    member __.GetOption(s) = getOption s this |> Option.bind (flip getOption other)
    member __.ModifyFunctionF(f) = modifyFunctionF (modifyFunctionF f other) this
    member __.ModifyTrampolineF(f) = modifyTrampolineF (modifyTrampolineF f other) this
    member __.ModifyOptionF(f) = modifyOptionF (modifyOptionF f other) this
    member __.ModifyListF(f) = modifyListF (modifyListF f other) this
    member __.ModifyChoiceF(f) = modifyChoiceF (modifyChoiceF f other) this
    member __.Modify(f) = modify (modify f other) this
  }

  let composeIso this other = composeOptional (InternalPIso.toOptional other) this

module Optional =

  let inline modify f (p: Optional<_, _>) = p.Modify(f)
  let inline modifyChoiceF f (p: Optional<_, _>) = p.ModifyChoiceF(f)
  let inline modifyOptionF f (p: Optional<_, _>) = p.ModifyOptionF(f)
  let inline modifyListF f (p: Optional<_, _>) = p.ModifyListF(f)
  let inline modifyTrampolineF f (p: Optional<_, _>) = p.ModifyTrampolineF(f)
  let inline getOrModify s (p: Optional<_, _>) = p.GetOrModify(s)
  let inline set b (p: Optional<_, _>) = p.Set(b)
  let inline getOption s (p: Optional<_, _>) = p.GetOption(s)

  let toSetter this = { PSetter = this.POptional |> POptional.toSetter }
  let toTraversal this = { PTraversal = this.POptional |> POptional.toTraversal }

  let sum this other = { POptional = POptional.sum this other }
  let fst this = { POptional = POptional.fst this }
  let snd this = { POptional = POptional.snd this }

  let optional getOrModify set = { POptional = POptional.poptional getOrModify set }

  let composeIso this other = { POptional = POptional.composeIso this.POptional other.PIso }

module PLens =

  let inline get s (l: PLens<_, _, _, _>) = l.Get(s)
  let inline set b (l: PLens<_, _, _, _>) = l.Set(b)
  let inline modify f (l: PLens<_, _, _, _>) = l.Modify(f)
  let inline modifyFunctionF f (l: PLens<_, _, _, _>) = l.ModifyFunctionF(f)
  let inline modifyChoiceF f (l: PLens<_, _, _, _>) = l.ModifyChoiceF(f)
  let inline modifyListF f (l: PLens<_, _, _, _>) = l.ModifyListF(f)
  let inline modifyOptionF f (l: PLens<_, _, _, _>) = l.ModifyOptionF(f)
  let inline modifyTrampolineF f (l: PLens<_, _, _, _>) = l.ModifyTrampolineF(f)

  let plens get set = { new PLens<_, _, _, _> with
    member __.Get(s) = get s
    member __.Set(b) = set b
    member __.ModifyFunctionF(f) = fun s ->
      f (get s) >> fun b -> set b s
    member __.ModifyChoiceF(f) = fun s ->
      f (get s) |> Choice.rightMap (fun a -> set a s)
    member __.ModifyTrampolineF(f) = fun s ->
      f (get s) |> Trampoline.map (fun a -> set a s)
    member __.ModifyListF(f) = fun s ->
      f (get s) |> List.map (fun a -> set a s)
    member __.ModifyOptionF(f) = fun s ->
      f (get s) |> Option.map (fun a -> set a s)
    member this.Modify(f) = fun s -> this.Set(f (get s)) s
  }

  let sum this other = 
    plens (Choice.fold (flip get this) (flip get other)) (fun b e -> Choice.bimap (set b this) (set b other) e)

  let toFold l = { new Fold<_, _>() with
    member __.FoldMap(_, f) = fun s -> f (get s l) }

  let toGetter l = { new Getter<_, _>() with
    member __.Get(s) = get s l }

  let toSetter l = { new PSetter<_, _, _, _> with
    member __.Set(b) = set b l
    member __.Modify(f) = modify f l }

  let toTraversal l = { new PTraversal<_, _, _, _> with
    member __.ModifyFunctionF(f) = modifyFunctionF f l
    member __.ModifyTrampolineF(f) = modifyTrampolineF f l
    member __.ModifyOptionF(f) = modifyOptionF f l
    member __.ModifyListF(f) = modifyListF f l
    member __.ModifyChoiceF(f) = modifyChoiceF f l
    member __.Modify(f) = modify f l
    member __.FoldMap(_, f) = fun s -> get s l |> f
  }

  let toOptional l = { new POptional<_, _, _, _> with
    member __.Set(b) = set b l
    member __.GetOrModify(s) = Choice2Of2 (get s l)
    member __.GetOption(s) = Some (get s l)
    member __.ModifyFunctionF(f) = modifyFunctionF f l
    member __.ModifyTrampolineF(f) = modifyTrampolineF f l
    member __.ModifyOptionF(f) = modifyOptionF f l
    member __.ModifyListF(f) = modifyListF f l
    member __.ModifyChoiceF(f) = modifyChoiceF f l
    member __.Modify(f) = modify f l
  }

  let composeLens this other = { new PLens<_, _, _, _> with
    member __.Get(s) = get s this |> flip get other
    member __.Set(b) = set b other |> flip modify this
    member __.ModifyFunctionF(f) = modifyFunctionF (modifyFunctionF f other) this
    member __.ModifyChoiceF(f) = modifyChoiceF (modifyChoiceF f other) this
    member __.ModifyTrampolineF(f) = modifyTrampolineF (modifyTrampolineF f other) this
    member __.ModifyListF(f) = modifyListF (modifyListF f other) this
    member __.ModifyOptionF(f) = modifyOptionF (modifyOptionF f other) this
    member this.Modify(f) = modify (modify f other) this
  }

  let composeFold this other = toFold this |> Fold.composeFold other
  let composeSetter this other = toSetter this |> flip PSetter.composeSetter other

module Lens =

  let inline get s (l: Lens<_, _>) = l.Get(s)
  let inline set b (l: Lens<_, _>) = l.Set(b)
  let inline modify f (l: Lens<_, _>) = l.Modify(f)
  let inline modifyFunctionF f (l: Lens<_, _>) = l.ModifyFunctionF(f)
  let inline modifyChoiceF f (l: Lens<_, _>) = l.ModifyChoiceF(f)
  let inline modifyListF f (l: Lens<_, _>) = l.ModifyListF(f)
  let inline modifyOptionF f (l: Lens<_, _>) = l.ModifyOptionF(f)
  let inline modifyTrampolineF f (l: Lens<_, _>) = l.ModifyTrampolineF(f)
  let inline sum a b = { PLens = PLens.sum a.PLens b.PLens }
  let inline composeSetter a b = { PSetter = PLens.composeSetter a.PLens b.PSetter }
  let inline composeLens a b = { PLens = PLens.composeLens a.PLens b.PLens }
  let inline toSetter l = { PSetter = PLens.toSetter l.PLens }
  let inline toTraversal l = { PTraversal = PLens.toTraversal l.PLens }
  let inline lens get set = { PLens = PLens.plens get set }

module PPrism =

  let inline getOrModify s (p: PPrism<_, _, _, _>) = p.GetOrModify(s)
  let inline getOption s (p: PPrism<_, _, _, _>) = p.GetOption(s)
  let inline reverseGet s (p: PPrism<_, _, _, _>) = p.ReverseGet(s)

  let modifyFunctionF f p = fun s  ->
    getOrModify s p
    |> Choice.fold konst (fun a -> f a >> flip reverseGet p)

  let modifyChoiceF f p = fun s  ->
    getOrModify s p
    |> Choice.fold Choice2Of2 (f >> Choice.rightMap (flip reverseGet p))

  let modifyTrampolineF f p = fun s  ->
    getOrModify s p
    |> Choice.fold Trampoline.purely (f >> Trampoline.map (flip reverseGet p))

  let modifyListF f p = fun s  ->
    getOrModify s p
    |> Choice.fold List.singleton (f >> List.map (flip reverseGet p))

  let modifyOptionF f p = fun s  ->
    getOrModify s p
    |> Choice.fold Some (f >> Option.map (flip reverseGet p))

  let modify f p = fun s  -> getOrModify s p |> Choice.fold id (f >> flip reverseGet p)

  let modifyOption f p = fun s  -> getOption s p |> Option.map (f >> flip reverseGet p)

  let set b p = modify (konst b) p
  let setOption b p = modifyOption (konst b) p

  let isMatching s p = getOption s p |> Option.isSome

  let re p = Getter.getter (flip reverseGet p)

  let toFold p = { new Fold<_, _>() with
    member __.FoldMap(m, f) = fun s ->
      getOption s p |> Option.map f |> Option.getOrElse (Monoid.zero m) }

  let toSetter p = { new PSetter<_, _, _, _> with
    member __.Modify(f) = modify f p
    member __.Set(b) = set b p }

  let toTraversal p = { new PTraversal<_, _, _, _> with
    member __.ModifyFunctionF(f) = modifyFunctionF f p
    member __.ModifyTrampolineF(f) = modifyTrampolineF f p
    member __.ModifyOptionF(f) = modifyOptionF f p
    member __.ModifyListF(f) = modifyListF f p
    member __.ModifyChoiceF(f) = modifyChoiceF f p
    member __.Modify(f) = modify f p
    member __.FoldMap(m, f) = fun s ->
      getOption s p |> Option.map f |> Option.getOrElse (Monoid.zero m)
  }

  let toOptional p = { new POptional<_, _, _, _> with
    member __.Set(b) = set b p
    member __.GetOrModify(s) = getOrModify s p
    member __.GetOption(s) = getOption s p
    member __.ModifyFunctionF(f) = modifyFunctionF f p
    member __.ModifyTrampolineF(f) = modifyTrampolineF f p
    member __.ModifyOptionF(f) = modifyOptionF f p
    member __.ModifyListF(f) = modifyListF f p
    member __.ModifyChoiceF(f) = modifyChoiceF f p
    member __.Modify(f) = modify f p
  }

  let pprism getOrModify reverseGet = { new PPrism<_, _, _, _> with
    member __.GetOrModify(s) = getOrModify s
    member __.ReverseGet(b) = reverseGet b
    member __.GetOption(s) = getOrModify s |> Choice.rightToOption }

  let composeFold this other = toFold this |> Fold.composeFold other
  let composeSetter this other = PSetter.composeSetter (toSetter this) other
  let composeOptional this other = POptional.composeOptional (toOptional this) other
  let composeLens this other = POptional.composeOptional (toOptional this) (PLens.toOptional other)

  let composePrism this other = { new PPrism<_, _, _, _> with
    member __.GetOrModify(s) =
      getOrModify s this
      |> Choice.rightBind (flip getOrModify other >> Choice.bimap (fun b -> set b this s) id)
    member __.ReverseGet(d) = reverseGet (reverseGet d other) this
    member __.GetOption(s) = getOption s this |> Option.bind (flip getOption other) }

module Prism =

  let inline getOrModify s (p: Prism< _, _>) = p.GetOrModify(s)
  let inline getOption s (p: Prism<_, _>) = p.GetOption(s)
  let inline reverseGet s (p: Prism<_, _>) = p.ReverseGet(s)

  let toSetter p = { PSetter = PPrism.toSetter p.PPrism }
  let toTraversal p = { PTraversal = PPrism.toTraversal p.PPrism }
  let toOptional p = { POptional = PPrism.toOptional p.PPrism }

  let composeSetter this other = { PSetter = PPrism.composeSetter this.PPrism other.PSetter }
  let composeOptional this other = { POptional = PPrism.composeOptional this.PPrism other.POptional }
  let composePrism this other = { PPrism = PPrism.composePrism this.PPrism other.PPrism }
  let composeLens this other = { POptional = PPrism.composeLens this.PPrism other.PLens }

module PIso =

  open InternalPIso

  let inline get s (i: PIso<_, _, _, _>) = i.Get(s)
  let inline reverseGet b (i: PIso<_, _, _, _>) = i.ReverseGet(b)
  let inline reverse (i: PIso<_, _, _, _>) = i.Reverse()

  let modifyFunctionF f i = modifyFunctionF f i
  let modifyChoiceF f i = modifyChoiceF f i
  let modifyTrampolineF f i = modifyTrampolineF f i
  let modifyOptionF f i = modifyOptionF f i
  let modifyListF f i = modifyListF f i
  let modify f i = modify f i
  let set b i = konst (reverseGet b i)

  let piso get reverseGet = piso get reverseGet

  let pid<'S, 'T> = pid<'S, 'T>

  let toFold p = toFold p
  let toGetter p = toGetter p
  let toSetter p = toSetter p
  let toPrism p = toPrism p
  let toTraversal p = toTraversal p
  let toOptional p = toOptional p
  let toLens p = toLens p

  let product other this =
    piso (fun (ss11, ss12) -> (get ss11 this, get ss12 other)) (fun (bb11, bb12) -> (reverseGet bb11 this, reverseGet bb12 other))

  let fst p = piso (fun (sc1, sc2) -> (get sc1 p, sc2)) (fun (bc1, bc2) -> (reverseGet bc1 p, bc2))
  let snd p = piso (fun (cs1, cs2) -> (cs1, get cs2)) (fun (cb1, cb2) -> (cb1, reverseGet cb2 p))

  let composeFold this other = toFold this |> Fold.composeFold other
  let composeGetter this other = Getter.composeGetter (toGetter this) other
  let composeSetter this other = PSetter.composeSetter (toSetter this) other
  let composeOptional this other = POptional.composeOptional (toOptional this) other
  let composePrism this other = PPrism.composePrism (toPrism this) other
  let composeLens this other = PLens.composeLens (toLens this) other

  let composeIso this other = { new PIso<_, _, _, _> with
    member __.Get(s) = get (get s this) other
    member __.ReverseGet(b) = reverseGet (reverseGet b other) this
    member x.Reverse() = { new PIso<_, _, _, _> with
      member __.Get(s) = reverseGet (reverseGet s other) this
      member __.ReverseGet(b) = get (get b this) other 
      member __.Reverse() = x
    }
  }

module Iso =

  let inline get s (i: Iso<_, _>) = i.Get(s)
  let inline reverseGet b (i: Iso<_, _>) = i.ReverseGet(b)
  let inline reverse (i: Iso<_, _>) = i.Reverse()

  let product other this = { PIso = PIso.product other.PIso this.PIso }
  let fst p = { PIso = PIso.fst p.PIso }
  let snd p = { PIso = PIso.snd p.PIso }

  let iso get reverseGet = { PIso = PIso.piso get reverseGet }
  let id<'S> = { PIso = PIso.pid<'S, 'S> }

  let toSetter p = { PSetter = PIso.toSetter p.PIso }
  let toTraversal p = { PTraversal = PIso.toTraversal p.PIso }
  let toOptional p = { POptional = PIso.toOptional p.PIso }
  let toPrism p = { PPrism = PIso.toPrism p.PIso }
  let toLens p = { PLens = PIso.toLens p.PIso }

  let composeSetter this other = { PSetter = PIso.composeSetter this.PIso other.PSetter }
  let composeOptional this other = { POptional = PIso.composeOptional this.PIso other.POptional }
  let composePrism this other = { PPrism = PIso.composePrism this.PIso other.PPrism }
  let composeLens this other = Lens.composeLens (toLens this) other
  let composeIso this other = { PIso = PIso.composeIso this.PIso other.PIso }
