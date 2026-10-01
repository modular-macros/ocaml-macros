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
