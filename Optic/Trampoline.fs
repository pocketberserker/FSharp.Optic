namespace Optic

[<AbstractClass>]
type Trampoline<'A>() =
  abstract member Bind: ('A -> Trampoline<'B>) -> Trampoline<'B>
  abstract member Fold: (Normal<'A> -> 'R) * (Codense<'A> -> 'R) -> 'R
  abstract member Box: unit -> Trampoline<obj>
  abstract member Resume: unit -> Choice<Lazy<Trampoline<'A>>, 'A>
  member this.Run() =
    let mutable current = this
    let mutable stop = false
    let mutable ret = None
    while not stop do
      match current.Resume() with
      | Choice1Of2 l -> current <- l.Value
      | Choice2Of2 r ->
        stop <- true
        ret <- Some r
    Option.get ret

and [<AbstractClass>] Normal<'A>() =
  inherit Trampoline<'A>()
  abstract member FoldNormal: ('A -> 'R) * (Lazy<Trampoline<'A>> -> 'R) -> 'R
  override this.Bind(f) = Codense.Codense(this, f)
  member this.BoxNormal() = { new Normal<obj>() with
    member __.FoldNormal(f, k) = this.FoldNormal(box >> f, fun t -> k (lazy t.Value.Box()))
    member __.Resume() =
      match this.Resume() with
      | Choice1Of2 l -> Choice1Of2(lazy l.Value.Box())
      | Choice2Of2 r -> Choice2Of2(box r)
    member __.Fold(n, gs) = this.Fold((fun x -> n (x.BoxNormal())), fun x -> gs (x.BoxCodense())) }
  override this.Box() = this.BoxNormal() :> Trampoline<obj>

and Suspend<'A>(suspension: Trampoline<'A>) =
  inherit Normal<'A>()
  override __.FoldNormal(_, k) = k (lazy suspension)
  override __.Resume() = Choice1Of2 (lazy suspension)
  override this.Fold(n, _) = n this

and Suspend =
  static member Suspend<'T>(a: unit -> Trampoline<'T>) = Suspend<'T>(a ()) :> Trampoline<'T>

and Pure<'A>(a: 'A) =
  inherit Normal<'A>()
  override __.FoldNormal(p, _) = p a
  override __.Resume() = Choice2Of2 a
  override this.Fold(n, _) = n this

and Codense<'A>(sub: Normal<obj>, cont: obj -> Trampoline<'A>) =
  inherit Trampoline<'A>()
  static member internal Codense(a: Normal<'A>, k: 'A -> Trampoline<'B>) =
    Codense(a.BoxNormal(), fun x -> unbox<'A> x |> k) :> Trampoline<_>
  member __.Sub = sub
  member __.Cont = cont
  override __.Bind(f: 'A -> Trampoline<'B>) =
    Codense.Codense(sub, fun o -> Suspend.Suspend<'B>(fun () -> (cont o).Bind(f)))
  override __.Box() = Codense(sub, cont >> fun t -> t.Box()) :> Trampoline<_>
  override __.Resume() =
    Choice1Of2(
      match sub.Resume() with
      | Choice1Of2 p ->
        let f (o: Normal<_>) = o.FoldNormal((fun o1 -> cont o1), fun t -> t.Value.Bind(cont))
        let g (c: Codense<_>) = Codense.Codense(c.Sub, fun o -> (c.Cont o).Bind(cont))
        lazy p.Value.Fold(f, g)
      | Choice2Of2 o -> lazy cont o)
  member __.BoxCodense() = Codense(sub, cont >> (fun x -> x.Box()))
  override this.Fold(_, gs) = gs this

module Trampoline =

  let purely a = Pure(a) :> Trampoline<_>

  let inline bind f (t: Trampoline<_>) = t.Bind(f)

  let map f t = bind (f >> purely) t

  let apply lf t = bind (fun f -> map f t) lf

  let inline run (t: Trampoline<_>) = t.Run()
