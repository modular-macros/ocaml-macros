(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2026 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* This module mirrors parts of the compiler's term representation
   (typing/ident.ml and lambda/lambda.mli); see the note at the top of
   the implementation. *)
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

(* [Lambda.lambda] *)
type lam

val const : 'a -> lam
(* [Lconst c], where [c] rebuilds the value: an immediate, a string, a
   boxed number, a float, a flat float block, or a block of these.
   Raises [Invalid_argument] on any other value, and does not terminate
   on a cyclic one. *)
