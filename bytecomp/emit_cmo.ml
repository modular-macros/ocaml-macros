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

let to_cmo ~source_file ~prefix ~required_globals lam =
  let unit_info =
    Unit_info.make ~check_modname:false ~source_file Impl prefix
  in
  let artifact = Unit_info.cmo unit_info in
  let filename = Unit_info.Artifact.filename artifact in
  let oc = open_out_bin filename in
  Misc.try_finally
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun () -> Misc.remove_file filename)
    (fun () ->
       lam
       |> Bytegen.compile_implementation (Unit_info.modname unit_info)
       |> Emitcode.to_file oc artifact ~required_globals)
