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

open Misc
open Format
open Cmo_format
open Asttypes

exception Load_failed

let check_consistency ppf filename cu =
  try
    List.iter
      (fun (name, crco) ->
       Env.add_import name;
       match crco with
         None -> ()
       | Some crc->
           Consistbl.check Env.crc_units name crc filename)
      cu.cu_imports
  with Consistbl.Inconsistency(name, user, auth) ->
    fprintf ppf "@[<hv 0>The files %s@ and %s@ \
                 disagree over interface %s@]@."
            user auth name;
    raise Load_failed

let load_compunit ic filename ppf compunit before_ld after_ld on_failure
    dependency =
  check_consistency ppf filename compunit;
  seek_in ic compunit.cu_pos;
  let code_size = compunit.cu_codesize + 8 in
  let code = Meta.static_alloc code_size in
  unsafe_really_input ic code 0 compunit.cu_codesize;
  Bytes.unsafe_set code compunit.cu_codesize (Char.chr Opcodes.opRETURN);
  String.unsafe_blit "\000\000\000\001\000\000\000" 0
                     code (compunit.cu_codesize + 1) 7;
  let initial_symtable = Symtable.current_state() in
  let fmt_static oc = function
    | Nonstatic -> output_string oc "Nonstatic"
    | Static -> output_string oc "Static"
  in
  Printf.fprintf stderr "%s loaded with dependency scheme %a -> %a\n%!"
    filename fmt_static (fst dependency) fmt_static (snd dependency);
  let unlifted_name = compunit.cu_name in
  let reloc_mapper = match dependency with
  | (Static, Nonstatic) -> (function
       | (Reloc_setglobal id, n) when Ident.name id = unlifted_name ->
           (Reloc_setglobal (Ident.create_persistent @@ "^" ^ unlifted_name), n)
       | other -> other
  )
  | (Nonstatic, Nonstatic) | (Static, Static) -> (fun x -> x)
  | _ -> assert false
  in
  let new_reloc = List.map reloc_mapper compunit.cu_reloc in
  Symtable.patch_object code new_reloc;
  Symtable.update_global_table();
  let events =
    if compunit.cu_debug = 0 then [| |]
    else begin
      seek_in ic compunit.cu_debug;
      [| input_value ic |]
    end in
  Meta.add_debug_info code code_size events;
  begin try
    before_ld ();
    ignore((Meta.reify_bytecode code code_size) ());
    after_ld ();
  with exn ->
    Symtable.restore_state initial_symtable;
    on_failure exn;
    raise Load_failed
  end

let rec load_file recursive ppf dependency name before_ld after_ld
    on_failure =
  let filename =
    try Some (find_in_path !Config.load_path name) with Not_found -> None
  in
  match filename with
  | None -> fprintf ppf "Cannot find file %s.@." name; false
  | Some filename ->
      let ic = open_in_bin filename in
      try
        let success = really_load_file recursive ppf name filename ic
              before_ld after_ld on_failure dependency
        in
        close_in ic;
        success
      with exn ->
        close_in ic;
        raise exn

and really_load_file recursive ppf name filename ic
    before_ld after_ld on_failure dependency =
  let buffer = really_input_string ic (String.length Config.cmo_magic_number) in
  try
    if buffer = Config.cmo_magic_number then begin
      let compunit_pos = input_binary_int ic in  (* Go to descriptor *)
      seek_in ic compunit_pos;
      let cu : compilation_unit = input_value ic in
      if recursive then
        load_deps ppf (snd dependency) cu.cu_reloc
          before_ld after_ld on_failure;
      load_compunit ic filename ppf cu before_ld after_ld on_failure dependency;
      true
    end else
      if buffer = Config.cma_magic_number then begin
        let toc_pos = input_binary_int ic in  (* Go to table of contents *)
        seek_in ic toc_pos;
        let lib = (input_value ic : library) in
        List.iter
          (fun dllib ->
            let name = Dll.extract_dll_name dllib in
            try Dll.open_dlls Dll.For_execution [name]
            with Failure reason ->
              fprintf ppf
                "Cannot load required shared library %s.@.Reason: %s.@."
                name reason;
              raise Load_failed)
          lib.lib_dllibs;
        List.iter
          (fun cu ->
            load_compunit ic filename ppf cu before_ld after_ld on_failure
              dependency)
          lib.lib_units;
        true
      end else begin
        fprintf ppf "File %s is not a bytecode object file.@." name;
        false
      end
  with Load_failed -> false

and load_deps ppf static_flag reloc before_ld after_ld on_failure =
  match static_flag with
  | Nonstatic ->
    List.iter
      (function
        | (Reloc_getglobal id, _) when not (Symtable.is_global_defined id) ->
            let file = Ident.name id ^ ".cmo" in
            begin match try Some (Misc.find_in_path_uncap !Config.load_path
                                    file)
                  with Not_found -> None
            with
            | None -> ()
            | Some file ->
                if not (load_file
                    true ppf (Nonstatic, Nonstatic) file before_ld
                    after_ld on_failure) then
                  raise Load_failed
            end
        | _ -> ()
      )
      reloc
  | Static ->
    List.iter
     (function
       | (Reloc_getglobal id, _) when not (Symtable.is_global_defined id) ->
           let name = Ident.name id in
           if Path.is_lifted (Path.Pident id) then
             let unlifted_name = Path.unlift_string @@ name in
             let file = unlifted_name ^ ".cmo" in
             begin match try Some (Misc.find_in_path_uncap !Config.load_path
                                     file)
                   with Not_found -> None
             with
             | None -> ()
             | Some file ->
                 if not (load_file
                     true ppf (Static, Nonstatic) file
                     before_ld after_ld on_failure) then
                   raise Load_failed
             end
           else
             let file = Ident.name id ^ ".cmm" in
             begin match try Some (Misc.find_in_path_uncap !Config.load_path
                                     file)
               with Not_found -> None
             with
             | None -> ()
             | Some file ->
                 if not (load_file
                     true ppf (Static, Static) file
                     before_ld after_ld on_failure) then
                   raise Load_failed
             end
       | _ -> ()
     )
     reloc

let load_file recursive ppf = load_file recursive ppf (Nonstatic, Nonstatic)

