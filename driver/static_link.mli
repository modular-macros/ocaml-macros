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

(** Compiling a unit that has top-level splices.

    The driver writes the static program of the unit, [M$static.cmo],
    and has [staticrun] load it in another process.  The static program
    evaluates the splices and writes the terms that fill the holes of
    the unit's run-time term, which the driver compiles as that of any
    unit, in bytecode or in native code.  The static program is bytecode
    in both cases. *)

val emit_macros_object :
  Compile_common.info -> Typedtree.implementation -> unit
(** Write the compile-time part of the unit as an object of its own,
    [prefix ^ "$macros.cmo"], which the static programs of the units
    that use it load. *)

val build_static_program :
  native:bool -> Compile_common.info -> Typedtree.implementation ->
  Lambda.program
(** Build the static program of the unit and run it, and give the
    unit's run-time term with its holes filled by the program.  If the
    run succeeds, or reports its own failure, the static program is
    removed; otherwise it is kept for inspection. *)

val implementation :
  native:bool ->
  Compile_common.info ->
  Typedtree.implementation ->
  fallback:(unit -> unit) ->
  finish:(Lambda.program -> unit) ->
  unit
(** Write the compile-time part of the unit if it has one.  Then, if the
    unit has a top-level splice, build its run-time term through its
    static program and pass it to [finish], the driver's code generation
    from a term; otherwise run [fallback], the driver's ordinary code
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
