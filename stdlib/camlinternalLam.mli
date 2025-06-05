module Ident : sig
    type t =
        Local of { name: string; stamp: int }
      | Scoped of { name: string; stamp: int; scope: int }
      | Global of string
      | Predef of { name: string; stamp: int }

    val compare : t -> t -> int
end
