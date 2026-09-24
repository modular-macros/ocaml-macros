module Ident : sig
    module Unscoped : sig
      type desc = { name: string; stamp: int }
      type state =
        | Udesc of desc
        | Ulink of t
      and t = { mutable state : state }

      val get_desc : t -> desc
    end

    type t =
        Local of { name: string; stamp: int }
      | Scoped of { name: string; stamp: int; scope: int }
      | Global of string
      | Predef of { name: string; stamp: int }
      | Unscoped of Unscoped.t

    val compare : t -> t -> int
end

type lam

val const : 'a -> lam

val var : Ident.t -> lam

val make_iarray : lam list -> lam

val letrec : (Ident.t * lam) list -> lam -> lam
