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

(** Static programs and macro objects: the machinery shared between the
    bytecode and native drivers.

    A module with top-level splices is compiled by emitting [M$static.cmo]
    and running it under the prebuilt RUNNER (staticrun: stdlib,
    compiler-libs and Dynlink, linked once with the compiler), which
    Dynlinks the unit's tail; the static object's initialisation evaluates
    the module's compile-time part and writes the terms that fill the holes
    of the module's run-time term, which the compiler compiles as that of
    any module.  The static program is always bytecode. *)

val emit_macros_object :
  Compile_common.info -> Typedtree.implementation -> unit
(** Write the compile-time part of the unit as an object of its own,
    [prefix ^ "$macros.cmo"], which the static programs of the units
    that use it load. *)

val build_static_program :
  native:bool -> Compile_common.info -> Typedtree.implementation ->
  Lambda.program
(** Build the module's static program and run it under the prebuilt
    runner, and give the module's run-time term with its holes filled by
    the program.  On success the static object and its object list are
    removed; a failed run keeps both for inspection. *)

val implementation :
  native:bool ->
  Compile_common.info ->
  Typedtree.implementation ->
  fallback:(unit -> unit) ->
  finish:(Lambda.program -> unit) ->
  unit
(** Write the compile-time part of the unit if it has one.  Then, if the
    unit has compile-time evaluation to do (a top-level splice, or a
    template application), build its run-time term through its static
    program and pass it to [finish], the driver's code generation from a
    term; otherwise run [fallback], the driver's ordinary code
    generation. *)

(** {2 Error report} *)

type error =
  | Not_a_bytecode_object of string
  | Static_archive_not_found of string
  | Static_archives_inconsistent of Linkdeps.error
  | Static_runner_not_found of string
  | Static_link_failed of string
  | Macro_object_not_found of { file : string; unit_name : string }
  | Static_program_failed of { base : string; status : int }
  | Static_program_no_holes of { base : string; file : string }
      (** The static program exited with status 0 without writing the
          terms that fill the holes. *)
  | Static_program_reported of string
      (** A failure of compile-time evaluation that the static program
          reported itself *)

exception Error of Location.t * error
(** The location is that of the unit, except for
    [Static_program_reported], whose location the static program
    gives. *)
