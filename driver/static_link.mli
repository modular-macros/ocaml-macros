(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

val should_emit_macros_object : exports:bool -> static_will_emit:bool -> bool

val emit_macros_object :
  Compile_common.info -> Typedtree.implementation -> unit

val build_static_program :
  native:bool -> Compile_common.info -> Typedtree.implementation -> unit

val implementation :
  native:bool ->
  Compile_common.info ->
  Typedtree.implementation ->
  fallback:(unit -> unit) ->
  unit

type error =
  | Not_a_bytecode_object of string
  | Static_archive_not_found of string
  | Static_runner_not_found of string
  | Static_link_failed of string
  | Macro_object_not_found of { file : string; unit_name : string }
  | Static_program_failed of { base : string; status : int }
  | Static_program_no_object of { base : string; file : string }
  | Static_program_reported of string

exception Error of Location.t * error
