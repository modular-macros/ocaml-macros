(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Olivier Nicole, Paris-Saclay University                  *)
(*                                                                        *)
(*                      Copyright 2016 OCaml Labs                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let run_static ppf lam =
  let splices =
    if Sys.backend_type = Sys.Bytecode then begin
      ignore (Symtable.init_static ());
      let (init_code, fun_code) = Bytegen.compile_phrase lam in
      let (code, code_size, reloc, _) =
        Emitcode.to_memory init_code fun_code
      in
      Bytelink.load_deps ppf 1 !Clflags.static_deps reloc;
      Symtable.patch_object 1 code reloc;
      Symtable.check_global_initialized 1 reloc;
      Symtable.update_global_table ();
      let splices = (Meta.reify_bytecode code code_size) () in
      (Obj.obj splices : Parsetree.expression array)
    end else if Sys.backend_type = Sys.Native then
      let open Filename in
      let (objfilename, oc) = open_temp_file ~mode:[Open_binary] "camlstatic" ".cmm" in
      let modulename = String.capitalize_ascii @@
        Filename.chop_extension @@ Filename.basename objfilename in
      let bytecode = Bytegen.compile_implementation modulename lam in
      Emitcode.to_file oc modulename objfilename bytecode;
      close_out oc;
      let execfilename = temp_file "camlstatic" "" in
      Bytelink.link ppf 1 (!Clflags.static_deps @ [objfilename]) execfilename;
      let resultfilename = temp_file "camlsplice" "" in
      let command =
        Config.standard_runtime ^ " " ^ execfilename ^ " > " ^ resultfilename
      in
      if Sys.command command <> 0 then
        Misc.fatal_error @@
          "Something went wrong when executing splice generation command: "
          ^ command;
      let ic = open_in resultfilename in
      let splices : Parsetree.expression array = Marshal.from_channel ic in
      close_in ic;
      splices
    else assert false (* other backends not supported *)
  in
  Symtable.reset ();
  Bytelink.reset ();
  splices

let load_static_deps ppf reloc =
  if Sys.backend_type = Sys.Bytecode then
    Bytelink.load_deps ppf 1 !Clflags.static_deps reloc
  else if Sys.backend_type = Sys.Native then
    () (* This function should not be used in native programs *)
  else assert false (* other backends not supported *)

