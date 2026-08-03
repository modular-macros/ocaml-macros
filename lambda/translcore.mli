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
   for the core language *)

open Asttypes
open Typedtree
open Lambda
open Debuginfo.Scoped_location

type splice_source =
  | Splices_in_place of (Typedtree.expression -> lambda)
  | Splices_in_slots of (int -> Typedtree.expression -> lambda)

val set_splice_source: splice_source option -> unit

val with_splice_source: splice_source option -> (unit -> 'a) -> 'a

type macro_env =
  { me_param : Ident.t;
    me_index : (Ident.t * int list) list }

val with_macro_env: macro_env option -> (unit -> 'a) -> 'a

val in_quotation: unit -> bool
val in_macro_body: unit -> bool

val reset_toplevel_template_modules: unit -> unit

val with_template_slot_index:
  (Ident.t * Ident.t) list option -> (unit -> 'a) -> 'a

val with_defining_macros:
  ?fns:Ident.t Ident.Map.t -> Ident.Set.t -> (unit -> 'a) -> 'a

val with_toplevel_mode: (unit -> 'a) -> 'a
val in_toplevel: unit -> bool

type native_slot_layout =
  { nsl_module : Ident.t;
    nsl_pos : Ident.t -> int option }

val with_native_slot_layout:
  native_slot_layout option -> (unit -> 'a) -> 'a
val current_native_slot_layout: unit -> native_slot_layout option
val name_slots_through_block: native_slot_layout -> Lambda.lambda -> Lambda.lambda

val with_template_batch_idents: Ident.Set.t option -> (unit -> 'a) -> 'a
val current_template_batch_idents: unit -> Ident.Set.t option
val in_template_batch_set: Ident.t -> bool

val register_toplevel_template_module: Ident.t -> unit
val is_toplevel_template_module: Ident.t -> bool
val toplevel_template_macro_transl:
  (scoped_location -> Env.t -> Path.t -> lambda) ref

val pure_module : module_expr -> let_kind

val transl_exp: scopes:scopes -> expression -> lambda
val transl_apply: scopes:scopes
                  -> ?tailcall:tailcall_attribute
                  -> ?inlined:inline_attribute
                  -> ?specialised:specialise_attribute
                  -> lambda -> (arg_label * apply_arg) list
                  -> scoped_location -> lambda
val transl_let: scopes:scopes -> ?in_structure:bool -> rec_flag
                -> value_binding list -> lambda -> lambda

val transl_extension_constructor: scopes:scopes ->
  Env.t -> Path.t option ->
  extension_constructor -> lambda

val transl_scoped_exp : scopes:scopes -> expression -> lambda

type error =
    Free_super_var
  | Unreachable_reached
  | Objects_in_staged_code

exception Error of Location.t * error

val check_no_objects : Typedtree.expression -> unit

val report_error: error Format_doc.format_printer
val report_error_doc: error Format_doc.printer

(* Forward declaration -- to be filled in by Translmod.transl_module *)
val transl_module :
      (scopes:scopes -> module_coercion -> Path.t option ->
       module_expr -> lambda) ref
val transl_struct_item :
      (scopes:scopes -> Ident.t list -> Path.t option ->
       structure_item -> (Ident.t list -> lambda) -> lambda) ref
val transl_object :
      (scopes:scopes -> Ident.t -> string list ->
       class_expr -> lambda) ref
