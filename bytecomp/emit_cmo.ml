(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let to_cmo ~source_file ~prefix ~imports ~flags ~primitives ~required_globals
      lam =
  let unit_info =
    Unit_info.make ~check_modname:false ~source_file Impl prefix
  in
  Env.import_crcs ~source:source_file imports;
  Translmod.primitive_declarations := primitives;
  Clflags.set_emitter_flags flags;
  let artifact = Unit_info.cmo unit_info in
  let filename = Unit_info.Artifact.filename artifact in
  let oc = open_out_bin filename in
  Misc.try_finally
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun () -> Misc.remove_file filename)
    (fun () ->
       lam
       |> Bytegen.compile_implementation (Unit_info.modname unit_info)
       |> (fun instrs ->
            if !Clflags.dump_instr then
              Printinstr.instrlist Format.err_formatter instrs;
            instrs)
       |> Emitcode.to_file oc artifact ~required_globals)
