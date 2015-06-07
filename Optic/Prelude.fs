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
