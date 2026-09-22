(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc
open Compile_common

let tool_name = "ocamlc"

let with_info =
  Compile_common.with_info ~native:false ~tool_name

let interface ~source_file ~output_prefix =
  let unit_info = Unit_info.make ~source_file Intf output_prefix in
  with_info ~dump_ext:"cmi" unit_info @@ fun info ->
  Compile_common.interface info

(** Bytecode compilation backend for .ml files. *)

let to_bytecode i Typedtree.{structure; coercion; _} =
  (structure, coercion)
  |> Profile.(record transl)
    (Translmod.transl_implementation (Unit_info.modname i.target))
  |> (fun { Lambda.code = lambda; required_globals } ->
       lambda
       |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.lambda
       |> Simplif.simplify_lambda
       |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.lambda
       |> fun lambda -> lambda, required_globals
    )

let emit_bytecode i (lambda, required_globals) =
  Profile.(record ~accumulate:true generate)
    (fun () ->
       Emit_cmo.to_cmo
         ~source_file:(Unit_info.source_file i.target)
         ~prefix:(Unit_info.prefix i.target)
         ~imports:(Env.imports ())
         ~flags:(Clflags.emitter_flags ())
         ~primitives:!Translmod.primitive_declarations
         ~required_globals lambda)
    ()

let implementation ~start_from ~source_file ~output_prefix =
  let backend info typed =
    Static_link.implementation ~native:false info typed
      ~fallback:(fun () -> emit_bytecode info (to_bytecode info typed))
  in
  let unit_info = Unit_info.make ~source_file Impl output_prefix in
  with_info ~dump_ext:"cmo" unit_info @@ fun info ->
  match (start_from : Clflags.Compiler_pass.t) with
  | Parsing -> Compile_common.implementation info ~backend
  | _ -> Misc.fatal_errorf "Cannot start from %s"
           (Clflags.Compiler_pass.to_string start_from)
