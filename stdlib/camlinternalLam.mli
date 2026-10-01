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

val var : Ident.t -> lam
(* [Lvar id] *)

val make_iarray : lam list -> lam
(* [Lprim (Pmakearray (Pgenarray, Immutable), els, Loc_unknown)] *)

val letrec : (Ident.t * lam) list -> lam -> lam
(* [Lletrec (bindings, body)].  Raises [Invalid_argument] unless each
   right-hand side is an [Lfunction]. *)

val is_function : lam -> bool
(* Whether the term is an [Lfunction], as a right-hand side of [letrec]
   must be *)

val fresh_exit : unit -> int
(* A static exit number that no other term uses: it is negative, and the
   compiler's are not. *)

val exit_to : int -> lam
(* [Lstaticraise (k, [])] *)

val catch : lam -> int -> lam -> lam
(* [catch body k handler] is [Lstaticcatch (body, (k, []), handler)]. *)

val switch : lam -> numconsts:int -> (int * lam) list -> lam option -> lam
(* A switch on an integer, which must be in [0, numconsts): it is not
   checked.  The last argument is the branch for an integer with no
   case, and is dropped if every integer has one.  Raises
   [Invalid_argument] if [numconsts] is not positive or exceeds 65535,
   the largest jump table of bytecode, if a case is duplicated or out of
   range, or if the cases are incomplete and there is no such branch. *)

val switch_intervals :
  lam -> numconsts:int -> ((int * int) list * lam) list -> lam option -> lam
(* As [switch], each case being a list of inclusive intervals.  A body
   is built once, and equal bodies are shared.  Raises
   [Invalid_argument] also on an interval that is empty or out of
   range, and on intervals that overlap. *)

val string_switch : lam -> (string * lam) list -> lam -> lam
(* A switch on a string, with a default.  Equal bodies are shared.
   Raises [Invalid_argument] on a duplicated string. *)
