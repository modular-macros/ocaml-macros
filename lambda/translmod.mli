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

val transl_implementation:
      string -> structure * module_coercion -> Lambda.program

(* The static program of a module with top-level splices, and the
   run-time term of the module, with a hole for each top-level splice.
   The static program runs the module's compile-time part and writes,
   to [prefix ^ "$static.holes"], the block of the terms that fill the
   holes ([Translquote.fill_holes]). *)
val transl_static_program:
      ?native:bool ->
      string -> structure * module_coercion ->
      source_file:string -> prefix:string ->
      Lambda.program * Lambda.program

(* Whether the structure defines anything at compile time *)
val exports_macros: structure -> bool

(* Whether the structure contains a template application: the
   driver must then build a static program, which evaluates the
   applications' dynamic components.  (The macro object never depends
   on that evaluation -- its application entries are record calls --
   so the driver always emits it.) *)
val has_template_applications: structure -> bool

(* The compile-time part of a unit, as a bytecode unit of its own *)
val transl_macros_object:
      string -> structure * module_coercion -> Lambda.program
val transl_store_phrases: string -> structure -> int * lambda
val transl_store_implementation:
      string -> structure * module_coercion -> Lambda.program

val transl_implementation_flambda:
  string -> structure * module_coercion -> Lambda.program

val transl_toplevel_definition: structure -> lambda

(* Whether a phrase of the toplevel has compile-time components or
   top-level splices.  If so it is translated by
   [transl_toplevel_phrase], and otherwise by
   [transl_toplevel_definition]. *)
val toplevel_phrase_has_compile_time_part: structure -> bool

(* The compile-time part of a phrase of the toplevel.  It stores the
   compile-time values that the phrase binds, and its value is the
   run-time term of the phrase, a [Lambda.lambda], to be closed with
   [close_toplevel_term] and run. *)
val transl_toplevel_phrase: structure -> lambda

(* Bind the free variables of a term to their values in the value table
   of the toplevel. *)
val close_toplevel_term: lambda -> lambda
val transl_package:
      Ident.t option list -> Ident.t -> module_coercion -> lambda
val transl_store_package:
      Ident.t option list -> Ident.t -> module_coercion -> int * lambda

val transl_package_flambda:
      Ident.t option list -> module_coercion -> int * lambda

val toplevel_name: Ident.t -> string

(* The key of the compile-time value of an identifier in the value
   table of the toplevel *)
val toplevel_macros_name: Ident.t -> string

(* The key of a function of type [Location.t -> unit] in the value table
   of the toplevel, which the compile-time part of a phrase calls with
   the location of a splice whose code raises *)
val toplevel_splice_failed_key: string

(* In the toplevel, the units that the directive #static_dir makes
   available to compile-time code are loaded for it under their names
   followed by [static_unit_suffix]. *)
val static_unit_suffix : string
val register_toplevel_shifted_units : Misc.Stdlib.String.Set.t -> unit
val nat_toplevel_name: Ident.t -> Ident.t * int

val primitive_declarations: Primitive.description list ref

type unsafe_component =
  | Unsafe_module_binding
  | Unsafe_functor
  | Unsafe_non_function
  | Unsafe_typext

type unsafe_info =
  | Unsafe of {
      reason:unsafe_component;
      loc:Location.t;
      path:Path.t
    }
  | Unnamed

type error =
  Circular_dependency of (Ident.t * unsafe_info) list
| Conflicting_inline_attributes
| Template_restriction of string
| Static_quotes_local of Ident.t

exception Error of Location.t * error

val report_error: Location.t -> error -> Location.error

val reset: unit -> unit
