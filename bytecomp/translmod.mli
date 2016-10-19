(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Translation from typed abstract syntax to lambda terms,
   for the module language *)

open Typedtree
open Lambda
open Asttypes

val transl_implementation:
  string -> static_flag -> structure * module_coercion -> Lambda.program
val transl_store_phrases: static_flag -> string -> structure -> int * lambda
val transl_store_implementation:
      static_flag -> string -> structure * module_coercion -> Lambda.program

val transl_implementation_flambda:
  string -> structure * module_coercion -> Lambda.program

val transl_toplevel_definition: static_flag -> structure -> lambda
val transl_package:
      Ident.t option list -> Ident.t -> module_coercion -> lambda
val transl_store_package:
      Ident.t option list -> Ident.t -> module_coercion -> int * lambda

(** Wrap a piece of lambda code so that, if the original code returned a value,
    the result of this function will execute the original code, marshal the
    returned value, print it on the standard output and return unit. Intended
    for wrapping of splice-producing code in [ocaml*.opt]. *)
val wrap_marshal : Lambda.lambda -> Lambda.lambda

val transl_package_flambda:
      Ident.t option list -> module_coercion -> int * lambda

val toplevel_name: Ident.t -> string
val nat_toplevel_name: Ident.t -> Ident.t * int
val close_toplevel_term: lambda * unit -> lambda

(* Used in translmod *)
val transl_structure: IdentSet.elt list -> module_coercion -> Path.t option
  -> Asttypes.static_flag -> (structure_item -> lambda -> lambda) -> Env.t
  -> structure_item list -> lambda * int
val field_path: Path.t option -> Ident.t -> Path.t option

val primitive_declarations: Primitive.description list ref

type error =
  Circular_dependency of Ident.t

exception Error of Location.t * error

val report_error: Format.formatter -> error -> unit

val reset: unit -> unit
