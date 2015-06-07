namespace Optic.Tests

open Persimmon.Dried
open UseTestNameByReflection
open Optic

module PreludeTest =

  let ``konst always return first value`` = property {
    apply (Prop.forAll (Arb.int, Arb.int) (fun a b -> konst a b = a))
  }
