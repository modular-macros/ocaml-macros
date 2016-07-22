(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Olivier Nicole, Paris-Saclay University                  *)
(*                                                                        *)
(*                      Copyright 2016 OCaml Labs                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** translstatic.ml: Translation of static declarations to lambda terms for the
    module language. *)

(** Translates to lambda terms all static bindings (i.e. bindings declared with
    the [static] keyword), ignore other declarations. *)
val transl_implementation : string -> Typedtree.structure ->
  Typedtree.module_coercion -> Lambda.lambda

(** Wrap a piece of lambda code so that, if the original code returned a value,
    the result of this function will execute the original code, marshal the
    returned value, print it on the standard output and return unit. Intended
    for wrapping of splice-producing code in [ocaml*.opt]. *)
val wrap_marshal : Lambda.lambda -> Lambda.lambda

val transl_toplevel_definition : Typedtree.structure -> Lambda.lambda

