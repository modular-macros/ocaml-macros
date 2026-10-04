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

(* The state of a translation: what the translation of a unit, and of
   the parts of it, depend on.  See the .ml for the meaning of each
   component. *)

type world =
  | Run_world      (* the object that the program runs *)
  | Compile_world  (* the compile-time part *)

(* The environment of the generator whose body is being translated: the
   parameter that receives it, the fields to project for each captured
   identifier, and the generators of its group, which share it *)
type macro_env =
  { me_param : Ident.t;
    me_index : (Ident.t * int list) list;
    me_group : Ident.Set.t }

(* The field each ident of the unit occupies in the module block of the
   native closure backend's store form.  While set, an environment access
   baked into returned code names the block field rather than the local,
   whose scope the store form does not extend over later items.  Given
   by Translmod for the compile-time part of a native static program. *)
type native_slot_layout =
  { nsl_module : Ident.t;
    nsl_pos : Ident.t -> int option }

type unit_state =
  { mutable compile_world_dummies : Ident.Set.t }

type context =
  { world : world;
    quotation_depth : int;
    macro_env : macro_env option;
    native_slot_layout : native_slot_layout option;
    splice_source : (context -> expression -> lambda_splice) option;
    closing : Ident.t Ident.Map.t;
    quoted_variables : Ident.Set.t ref option;
    unit_globals : Ident.t -> Ident.t;
    innermost_free_variables : Ident.Set.t;
    unit_state : unit_state }

(* The context in which a unit's translation starts, with registries of
   its own *)
val initial_context: unit -> context

(* Whether the expression being translated is quoted code *)
val in_quotation: context -> bool

(* The globals of the units that a term of compile-time code names, as
   [unit_globals] gives them *)
val rename_unit_globals: context -> lambda -> lambda

val pure_module : module_expr -> let_kind

val transl_exp: ctx:context -> scopes:scopes -> expression -> lambda
val transl_apply: ctx:context -> scopes:scopes
                  -> ?tailcall:tailcall_attribute
                  -> ?inlined:inline_attribute
                  -> ?specialised:specialise_attribute
                  -> lambda -> (arg_label * apply_arg) list
                  -> scoped_location -> lambda
val transl_let: ctx:context -> scopes:scopes -> ?in_structure:bool -> rec_flag
                -> value_binding list -> lambda -> lambda

(* The variables of run-time code that the quotations of the given
   compile-time code mention: those that the code it builds is closed
   over *)
val quoted_variables: ctx:context -> scopes:scopes -> expression -> Ident.Set.t

val transl_extension_constructor: scopes:scopes ->
  Env.t -> Path.t option ->
  extension_constructor -> lambda

val transl_scoped_exp : ctx:context -> scopes:scopes -> expression -> lambda

type error =
    Free_super_var
  | Unreachable_reached
  | Objects_in_staged_code

exception Error of Location.t * error

(* Reject objects and classes in the code of an expression that is
   quoted or runs at compile time, or with [~anywhere] in all of it, with
   a located error. *)
val check_no_objects : ?anywhere:bool -> Typedtree.expression -> unit

val report_error: error Format_doc.format_printer
val report_error_doc: error Format_doc.printer

(* Forward declaration -- to be filled in by Translmod.transl_module *)
val transl_module :
      (ctx:context -> scopes:scopes -> module_coercion -> Path.t option ->
       module_expr -> lambda) ref
val transl_struct_item :
      (ctx:context -> scopes:scopes -> Ident.t list -> Path.t option ->
       structure_item -> (Ident.t list -> lambda) -> lambda) ref
val transl_object :
      (ctx:context -> scopes:scopes -> Ident.t -> string list ->
       class_expr -> lambda) ref
