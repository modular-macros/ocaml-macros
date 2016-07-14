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
  let (init_code, fun_code) = Bytegen.compile_phrase lam in
  let (code, code_size, reloc, _) =
    Emitcode.to_memory init_code fun_code
  in
  Bytelink.load_deps ppf 1 !Clflags.static_deps reloc;
  Symtable.patch_object 1 code reloc;
  Symtable.check_global_initialized 1 reloc;
  Symtable.update_global_table ();
  Printf.printf "before reify\n%!";
  let splices = (Meta.reify_bytecode code code_size) () in
  Printf.printf "after reify\n%!";
  Symtable.reset ();
  (Obj.obj splices : Parsetree.expression array)

let load_static_deps ppf reloc =
  Bytelink.load_deps ppf 1 !Clflags.static_deps reloc

