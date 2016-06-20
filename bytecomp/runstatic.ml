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
  ignore (Symtable.init_toplevel ());
  Symtable.init_static ();
  Cmo_load.load_deps ppf Asttypes.Static reloc
    (fun () -> ())
    (fun () -> ())
    (fun exn -> raise exn);
  Symtable.patch_object 1 code reloc;
  Symtable.check_global_initialized 1 reloc;
  Symtable.update_global_table ();
  Printf.fprintf stderr "before reify\n%!";
  let splices = (Meta.reify_bytecode code code_size) () in
  Printf.fprintf stderr "after reify\n%!";
  Symtable.reset ();
  (Obj.obj splices : Parsetree.expression array)

