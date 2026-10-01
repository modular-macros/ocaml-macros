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

(* [const v] is the structured constant denoting [v].

   [v] must be built only from immediates, strings, boxed numbers, floats,
   flat float blocks and blocks of these; anything else (a closure, a lazy
   value) is a fatal error, the classification being CamlinternalLam's.
   Sharing is not preserved and cycles do not terminate. *)
val const : 'a -> Lambda.lambda

(* [quote_expression lam] is code that builds the term [lam]: it
   evaluates to a pair of the term and the set of its free variables.
   The binders of [lam] are renamed each time the code is evaluated, and
   the contents of each [Lsplice] in [lam], which evaluate to such a
   pair, are evaluated in its place. *)
val quote_expression : Lambda.lambda -> Lambda.lambda

(* Close the code of a top-level splice over the given variables, the
   run-time variables that it quotes: gives those that are not
   persistent, in a fixed order, the parameters that stand for them,
   and the code, which the function translates the splice to, given the
   renaming of the variables to the parameters.  The caller abstracts
   the code over the parameters and applies it, at the hole, to the
   variables. *)
val transl_close_function :
  Location.t -> Ident.Set.t -> (Ident.t Ident.Map.t -> Lambda.lambda) ->
  Ident.t list * Ident.t list * Lambda.lambda
(* Rename, in a term about to be quoted, the variables that the splice
   being translated is closed over, by the renaming that
   [transl_close_function] gives. *)
val close_quoted : Ident.t Ident.Map.t -> Lambda.lambda -> Lambda.lambda

(* Code that rebuilds the lambda term of a unit, with the contents of
   each hole in the place of the hole.  A subterm with no hole becomes
   a constant.  Debug events are removed. *)
val quote_module_lambda : Lambda.lambda -> Lambda.lambda

(* [quote_module_holes lam] is code that rebuilds [lam] as
   [quote_module_lambda lam] does, evaluating its splices in the same
   order, and gives the block of the terms that fill its holes, in the
   order of [fill_holes]. *)
val quote_module_holes : Lambda.lambda -> Lambda.lambda

(* [fill_holes lam contents] fills the i-th hole of [lam] with
   [contents.(i)], and reserves the numbers of the static exceptions
   of [contents]. *)
val fill_holes : Lambda.lambda -> Lambda.lambda array -> Lambda.lambda
