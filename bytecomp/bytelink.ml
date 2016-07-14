(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Link a set of .cmo files and produce a bytecode executable. *)

open Misc
open Config
open Cmo_format

type error =
    File_not_found of string
  | Not_an_object_file of string
  | Wrong_object_name of string
  | Symbol_error of string * Symtable.error
  | Inconsistent_import of string * string * string
  | Custom_runtime
  | File_exists of string
  | Cannot_open_dll of string
  | Not_compatible_32

exception Error of error

type link_action =
    Link_object of string * compilation_unit * Types.phase
      (* Name of .cmo file, descriptor of the unit, phase to load the unit
       * into. *)
  | Link_archive of string * compilation_unit list * Types.phase
      (* Name of .cma file, descriptors of the units to be linked, phase to
       * load the units into. *)

(* Add C objects and options from a library descriptor *)
(* Ignore them if -noautolink or -use-runtime or -use-prim was given *)

let lib_ccobjs = ref []
let lib_ccopts = ref []
let lib_dllibs = ref []

let add_ccobjs origin l =
  if not !Clflags.no_auto_link then begin
    if
      String.length !Clflags.use_runtime = 0
      && String.length !Clflags.use_prims = 0
    then begin
      if l.lib_custom then Clflags.custom_runtime := true;
      lib_ccobjs := l.lib_ccobjs @ !lib_ccobjs;
      let replace_origin =
        Misc.replace_substring ~before:"$CAMLORIGIN" ~after:origin
      in
      lib_ccopts := List.map replace_origin l.lib_ccopts @ !lib_ccopts;
    end;
    lib_dllibs := l.lib_dllibs @ !lib_dllibs
  end

(* A note on ccobj ordering:
   - Clflags.ccobjs is in reverse order w.r.t. what was given on the
        ocamlc command line;
   - l.lib_ccobjs is also in reverse order w.r.t. what was given on the
        ocamlc -a command line when the library was created;
   - Clflags.ccobjs is reversed just before calling the C compiler for the
        custom link;
   - .cma files on the command line of ocamlc are scanned right to left;
   - Before linking, we add lib_ccobjs after Clflags.ccobjs.
   Thus, for ocamlc a.cma b.cma obj1 obj2
   where a.cma was built with ocamlc -i ... obja1 obja2
     and b.cma was built with ocamlc -i ... objb1 objb2
   lib_ccobjs starts as [],
   becomes objb2 objb1 when b.cma is scanned,
   then obja2 obja1 objb2 objb1 when a.cma is scanned.
   Clflags.ccobjs was initially obj2 obj1.
   and is set to obj2 obj1 obja2 obja1 objb2 objb1.
   Finally, the C compiler is given objb1 objb2 obja1 obja2 obj1 obj2,
   which is what we need.  (If b depends on a, a.cma must appear before
   b.cma, but b's C libraries must appear before a's C libraries.)
*)

(* First pass: determine which units are needed *)

type global_status =
  | Missing
  | Required
  | Available of Ident.t list
  | Path of string * compilation_unit

let pp_lst ~sep pp_data fmt =
  let rec aux = function
    | [] -> ()
    | [ x ] -> pp_data fmt x
    | x :: r -> Format.fprintf fmt "%a%a" pp_data x sep (); aux r
  in aux

let fmt_global_status fmt = function
  | Missing -> Format.fprintf fmt "Missing"
  | Required -> Format.fprintf fmt "Required"
  | Available reqs -> Format.fprintf fmt "Available { %a }"
      (pp_lst ~sep:(fun fmt () -> Format.fprintf fmt "@\n") (fun fmt x -> Format.fprintf fmt "%s" (Ident.name x))) reqs;
  | Path (path, _) -> Format.fprintf fmt "Path %s" path

let globals = ref Ident.empty

let add_global status (rel, _pos) =
  match rel with
  | Reloc_setglobal id ->
      Format.eprintf "add_global %s -> %a\n%!" (Ident.name id) fmt_global_status status;
      globals := Ident.add id status !globals
  | _ -> ()

let add_global_id status id =
  Format.eprintf "add_global_id %s -> %a\n%!" (Ident.name id) fmt_global_status status;
  globals := Ident.add id status !globals

let global_status id =
  Ident.find_same id !globals

let is_required_global (rel, _pos) =
  match rel with
  | Reloc_setglobal id -> begin
      match global_status id with
      | Missing | Available _ -> false
      | Required | Path _ -> true
    end
  | _ -> false

let required_globals phase l =
  let rec loop acc = function
    | [] -> List.rev acc
    | (Reloc_getglobal id, _pos) :: rest
      when not (Symtable.is_global_defined (phase, id)) ->
        loop (id :: acc) rest
    | _ :: rest ->
        loop acc rest
  in
    loop [] l

let rec lift_reloc = function
  | [] -> []
  | (Reloc_getglobal id, pos) :: rem ->
      (Reloc_getglobal (Ident.lift_persistent id), pos) :: lift_reloc rem
  | (Reloc_setglobal id, pos) :: rem ->
      (Reloc_setglobal (Ident.lift_persistent id), pos) :: lift_reloc rem
  | x :: rem -> x :: lift_reloc rem

(* Add entries for the explicitely provided .cmo and .cma files. [phase] is the
 * phase the units will be loaded into (e.g. if linking a normal executable it
 * will be 0, if linking a temporary, macro-producing code it will be 1. *)
let scan_file phase obj_name tolink =
  let file_name =
    try
      find_in_path !load_path obj_name
    with Not_found ->
      raise(Error(File_not_found obj_name)) in
  let ic = open_in_bin file_name in
  let lift_globals = phase = 1 &&
    (Filename.check_suffix obj_name ".cmo"
    || Filename.check_suffix obj_name ".cma") in
  try
    let buffer = really_input_string ic (String.length cmo_magic_number) in
    if buffer = cmo_magic_number then begin
      (* This is a .cmo file. It must be linked in any case.
         Read the relocation information to see which modules it
         requires. *)
      let compunit_pos = input_binary_int ic in  (* Go to descriptor *)
      seek_in ic compunit_pos;
      let compunit = (input_value ic : compilation_unit) in
      close_in ic;
      let compunit =
        if lift_globals then
          { compunit with cu_reloc = lift_reloc compunit.cu_reloc }
        else compunit
      in
      List.iter (add_global Required) compunit.cu_reloc;
      Link_object(file_name, compunit, phase) :: tolink
    end
    else if buffer = cma_magic_number then begin
      (* This is an archive file. Each unit contained in it will be linked
         in only if needed. *)
      let pos_toc = input_binary_int ic in    (* Go to table of contents *)
      seek_in ic pos_toc;
      let toc = (input_value ic : library) in
      close_in ic;
      add_ccobjs (Filename.dirname file_name) toc;
      let toc =
        if lift_globals then
          { toc with lib_units =
              List.map (fun u -> {u with cu_reloc = lift_reloc u.cu_reloc})
                toc.lib_units }
        else toc
      in
      List.iter
          (fun compunit ->
            let status =
              if compunit.cu_force_link || !Clflags.link_everything then begin
                Printf.eprintf "case 1\n%!";
                Required
              end else begin
                Printf.eprintf "case 2\n%!";
                let required = required_globals phase compunit.cu_reloc in
                Available required
              end
            in
            List.iter (add_global status) compunit.cu_reloc)
          toc.lib_units;
      Link_archive(file_name, toc.lib_units, phase) :: tolink
    end
    else raise(Error(Not_an_object_file file_name))
  with
    End_of_file -> close_in ic; raise(Error(Not_an_object_file file_name))
  | x -> close_in ic; raise x

let scan_from_path phase id =
  let modname = Ident.unlift_string (Ident.name id) in
  let extension =
    if phase = 1 then
      if Ident.lifted id then ".cmo" else ".cmm"
    else if phase = 0 then
      ".cmo"
    else assert false
  in
  match
    find_in_path_uncap !load_path (modname ^ extension)
  with
  | exception Not_found ->
      add_global_id Missing id;
      []
  | file_name ->
      let ic = open_in_bin file_name in
        try
          let buffer =
            really_input_string ic (String.length cmo_magic_number)
          in
          if buffer = cmo_magic_number then begin
            let compunit_pos = input_binary_int ic in
            (* Go to descriptor *)
            seek_in ic compunit_pos;
            let compunit = (input_value ic : compilation_unit) in
            close_in ic;
            let compunit =
              if phase = 1 && Ident.lifted id then
                { compunit with cu_reloc = lift_reloc compunit.cu_reloc }
              else compunit
            in
            let status = Path(file_name, compunit) in
            List.iter (add_global status) compunit.cu_reloc;
            required_globals phase compunit.cu_reloc
          end
          else raise(Error(Not_an_object_file file_name))
        with
        | End_of_file ->
            close_in ic; raise(Error(Not_an_object_file file_name))
        | x -> close_in ic; raise x

type to_scan =
    Objects of link_action list
  | Reloc_info of (Cmo_format.reloc_info * int) list

let scan_path phase tolink =
  let rec complete = function
    | [] -> ()
    | required :: rest ->
        Printf.eprintf "complete called on %s\n%!" (Ident.name required);
        Format.eprintf "case %a\n%!" fmt_global_status (global_status required);
        match global_status required with
        | Missing | Required | Path _ ->
            complete rest
        | Available reqs ->
            add_global_id Required required;
            List.iter (fun id -> Printf.eprintf "add %s to idents being scanned as dependency of %s\n%!" (Ident.name id) (Ident.name required)) reqs;
            complete (List.rev_append reqs rest)
        | exception Not_found ->
            Printf.eprintf "case Not_found\n%!";
            let reqs = scan_from_path phase required in
            complete (List.rev_append reqs rest)
  in
  match tolink with
  | Objects tolink ->
      List.iter
        (function
         | Link_archive (_, _, _) -> ()
         | Link_object(_, compunit, _) ->
             complete (required_globals phase compunit.cu_reloc))
        tolink
  | Reloc_info reloc ->
      complete (required_globals phase reloc)

let complete_link_actions phase tolink =
  let rec expand_ids = function
    | [] -> []
    | required :: rest ->
        match global_status required with
        | Missing | Required -> expand_ids rest
        | Path(file_name, compunit) ->
            add_global_id Required required;
            let required = required_globals phase compunit.cu_reloc in
            let prefix = expand_ids required in
            let action = Link_object(file_name, compunit, phase) in
            List.concat [prefix; [action]; expand_ids rest]
        | Available _ -> assert false
        | exception Not_found ->
            Printf.eprintf "%s not found\n%!" (Ident.name required);
            assert (Symtable.is_global_defined (phase, required));
            expand_ids rest
  in
  let rec expand_compunits = function
    | [] -> []
    | compunit :: rest ->
        let required = required_globals phase compunit.cu_reloc in
        let prefix = expand_ids required in
         prefix @ expand_compunits rest
  in
  let rec expand_actions = function
    | [] -> []
    | Link_object(_, compunit, _) as action :: rest ->
        let required = required_globals phase compunit.cu_reloc in
        let prefix = expand_ids required in
        List.concat [prefix; [action]; expand_actions rest]
    | Link_archive(file_name, compunits, phase) :: rest ->
        let compunits =
          List.filter
            (fun compunit ->
              List.exists is_required_global compunit.cu_reloc)
            compunits
        in
        let prefix = expand_compunits compunits in
        let action = Link_archive(file_name, compunits, phase) in
        List.concat [prefix; [action]; expand_actions rest]
  in
  expand_actions tolink

(* Second pass: link in the required units *)

(* Consistency check between interfaces *)

let crc_interfaces = Consistbl.create ()
let interfaces = ref ([] : string list)
let implementations_defined = ref ([] : (string * string) list)

let check_consistency ppf file_name cu =
  begin try
    List.iter
      (fun (name, crco) ->
        interfaces := name :: !interfaces;
        match crco with
          None -> ()
        | Some crc ->
            if name = cu.cu_name
            then Consistbl.set crc_interfaces name crc file_name
            else Consistbl.check crc_interfaces name crc file_name)
      cu.cu_imports
  with Consistbl.Inconsistency(uname, user, auth) ->
    raise(Error(Inconsistent_import(uname, user, auth)))
  end;
  begin try
    let source = List.assoc cu.cu_name !implementations_defined in
    Location.print_warning (Location.in_file file_name) ppf
      (Warnings.Multiple_definition(cu.cu_name,
                                    Location.show_filename file_name,
                                    Location.show_filename source))
  with Not_found -> ()
  end;
  implementations_defined :=
    (cu.cu_name, file_name) :: !implementations_defined

let extract_crc_interfaces () =
  Consistbl.extract !interfaces crc_interfaces

let clear_crc_interfaces () =
  Consistbl.clear crc_interfaces;
  interfaces := []

(* Record compilation events *)

let debug_info = ref ([] : (int * Instruct.debug_event list * string list) list)

(* Link in a compilation unit *)

let link_compunit ppf phase output_fun currpos_fun inchan file_name compunit =
  check_consistency ppf file_name compunit;
  seek_in inchan compunit.cu_pos;
  let code_block = LongString.input_bytes inchan compunit.cu_codesize in
  Symtable.ls_patch_object phase code_block compunit.cu_reloc;
  if !Clflags.debug && compunit.cu_debug > 0 then begin
    seek_in inchan compunit.cu_debug;
    let debug_event_list : Instruct.debug_event list = input_value inchan in
    let debug_dirs : string list = input_value inchan in
    let file_path = Filename.dirname (Location.absolute_path file_name) in
    let debug_dirs =
      if List.mem file_path debug_dirs
      then debug_dirs
      else file_path :: debug_dirs in
    debug_info := (currpos_fun(), debug_event_list, debug_dirs) :: !debug_info
  end;
  Array.iter output_fun code_block;
  if !Clflags.link_everything then
    List.iter Symtable.require_primitive compunit.cu_primitives

(* Link in a .cmo file *)

let link_object ppf phase output_fun currpos_fun file_name compunit =
  let inchan = open_in_bin file_name in
  try
    link_compunit ppf phase output_fun currpos_fun inchan file_name compunit;
    close_in inchan
  with
    Symtable.Error msg ->
      close_in inchan; raise(Error(Symbol_error(file_name, msg)))
  | x ->
      close_in inchan; raise x

(* Link in a .cma file *)

let link_archive ppf phase output_fun currpos_fun file_name units_required =
  let inchan = open_in_bin file_name in
  try
    List.iter
      (fun cu ->
         let name = file_name ^ "(" ^ cu.cu_name ^ ")" in
         try
           link_compunit ppf phase output_fun currpos_fun inchan name cu
         with Symtable.Error msg ->
           raise(Error(Symbol_error(name, msg))))
      units_required;
    close_in inchan
  with x -> close_in inchan; raise x

(* Link in a .cmo or .cma file *)

let link_file ppf output_fun currpos_fun = function
    Link_object(file_name, unit, phase) ->
      link_object ppf phase output_fun currpos_fun file_name unit
  | Link_archive(file_name, units, phase) ->
      link_archive ppf phase output_fun currpos_fun file_name units

(* Output the debugging information *)
(* Format is:
      <int32>          number of event lists
      <int32>          offset of first event list
      <output_value>   first event list
      ...
      <int32>          offset of last event list
      <output_value>   last event list *)

let output_debug_info oc =
  output_binary_int oc (List.length !debug_info);
  List.iter
    (fun (ofs, evl, debug_dirs) ->
      output_binary_int oc ofs;
      output_value oc evl;
      output_value oc debug_dirs)
    !debug_info;
  debug_info := []

(* Output a list of strings with 0-termination *)

let output_stringlist oc l =
  List.iter (fun s -> output_string oc s; output_byte oc 0) l

(* Transform a file name into an absolute file name *)

let make_absolute file =
  if Filename.is_relative file
  then Filename.concat (Sys.getcwd()) file
  else file

(* Create a bytecode executable file *)

let link_bytecode ppf tolink exec_name standalone =
  (* Avoid the case where the specified exec output file is the same as
     one of the objects to be linked *)
  List.iter (function
    | Link_object(file_name, _, _) when file_name = exec_name ->
      raise (Error (Wrong_object_name exec_name));
    | _ -> ()) tolink;
  Misc.remove_file exec_name; (* avoid permission problems, cf PR#1911 *)
  let outchan =
    open_out_gen [Open_wronly; Open_trunc; Open_creat; Open_binary]
                 0o777 exec_name in
  try
    if standalone then begin
      (* Copy the header *)
      try
        let header =
          if String.length !Clflags.use_runtime > 0
          then "camlheader_ur" else "camlheader" ^ !Clflags.runtime_variant in
        let inchan = open_in_bin (find_in_path !load_path header) in
        copy_file inchan outchan;
        close_in inchan
      with Not_found | Sys_error _ -> ()
    end;
    Bytesections.init_record outchan;
    (* The path to the bytecode interpreter (in use_runtime mode) *)
    if String.length !Clflags.use_runtime > 0 then begin
      output_string outchan ("#!" ^ (make_absolute !Clflags.use_runtime));
      output_char outchan '\n';
      Bytesections.record outchan "RNTM"
    end;
    (* The bytecode *)
    let start_code = pos_out outchan in
    Symtable.init();
    clear_crc_interfaces ();
    let sharedobjs = List.map Dll.extract_dll_name !Clflags.dllibs in
    let check_dlls = standalone && Config.target = Config.host in
    if check_dlls then begin
      (* Initialize the DLL machinery *)
      Dll.init_compile !Clflags.no_std_include;
      Dll.add_path !load_path;
      try Dll.open_dlls Dll.For_checking sharedobjs
      with Failure reason -> raise(Error(Cannot_open_dll reason))
    end;
    let output_fun = output_bytes outchan
    and currpos_fun () = pos_out outchan - start_code in
    List.iter (link_file ppf output_fun currpos_fun) tolink;
    if check_dlls then Dll.close_all_dlls();
    (* The final STOP instruction *)
    output_byte outchan Opcodes.opSTOP;
    output_byte outchan 0; output_byte outchan 0; output_byte outchan 0;
    Bytesections.record outchan "CODE";
    (* DLL stuff *)
    if standalone then begin
      (* The extra search path for DLLs *)
      output_stringlist outchan !Clflags.dllpaths;
      Bytesections.record outchan "DLPT";
      (* The names of the DLLs *)
      output_stringlist outchan sharedobjs;
      Bytesections.record outchan "DLLS"
    end;
    (* The names of all primitives *)
    Symtable.output_primitive_names outchan;
    Bytesections.record outchan "PRIM";
    (* The table of global data *)
    begin try
      Marshal.to_channel outchan (Symtable.initial_global_table())
          (if !Clflags.bytecode_compatible_32
           then [Marshal.Compat_32] else [])
    with Failure _ ->
      raise (Error Not_compatible_32)
    end;
    Bytesections.record outchan "DATA";
    (* The map of global identifiers *)
    Symtable.output_global_map outchan;
    Bytesections.record outchan "SYMB";
    (* CRCs for modules *)
    output_value outchan (extract_crc_interfaces());
    Bytesections.record outchan "CRCS";
    (* Debug info *)
    if !Clflags.debug then begin
      output_debug_info outchan;
      Bytesections.record outchan "DBUG"
    end;
    (* The table of contents and the trailer *)
    Bytesections.write_toc_and_trailer outchan;
    close_out outchan
  with x ->
    close_out outchan;
    remove_file exec_name;
    raise x

(* Output a string as a C array of unsigned ints *)

let output_code_string_counter = ref 0

let output_code_string outchan code =
  let pos = ref 0 in
  let len = Bytes.length code in
  while !pos < len do
    let c1 = Char.code(Bytes.get code !pos) in
    let c2 = Char.code(Bytes.get code (!pos + 1)) in
    let c3 = Char.code(Bytes.get code (!pos + 2)) in
    let c4 = Char.code(Bytes.get code (!pos + 3)) in
    pos := !pos + 4;
    Printf.fprintf outchan "0x%02x%02x%02x%02x, " c4 c3 c2 c1;
    incr output_code_string_counter;
    if !output_code_string_counter >= 6 then begin
      output_char outchan '\n';
      output_code_string_counter := 0
    end
  done

(* Output a string as a C string *)

let output_data_string outchan data =
  let counter = ref 0 in
  for i = 0 to String.length data - 1 do
    Printf.fprintf outchan "%d, " (Char.code(data.[i]));
    incr counter;
    if !counter >= 12 then begin
      output_string outchan "\n";
      counter := 0
    end
  done

(* Output a debug stub *)

let output_cds_file outfile =
  Misc.remove_file outfile;
  let outchan =
    open_out_gen [Open_wronly; Open_trunc; Open_creat; Open_binary]
      0o777 outfile in
  try
    Bytesections.init_record outchan;
    (* The map of global identifiers *)
    Symtable.output_global_map outchan;
    Bytesections.record outchan "SYMB";
    (* Debug info *)
    output_debug_info outchan;
    Bytesections.record outchan "DBUG";
    (* The table of contents and the trailer *)
    Bytesections.write_toc_and_trailer outchan;
    close_out outchan
  with x ->
    close_out outchan;
    remove_file outfile;
    raise x

(* Output a bytecode executable as a C file *)

let link_bytecode_as_c ppf tolink outfile =
  let outchan = open_out outfile in
  begin try
    (* The bytecode *)
    output_string outchan "\
#ifdef __cplusplus\
\nextern \"C\" {\
\n#endif\
\n#include <caml/mlvalues.h>\
\nCAMLextern void caml_startup_code(\
\n           code_t code, asize_t code_size,\
\n           char *data, asize_t data_size,\
\n           char *section_table, asize_t section_table_size,\
\n           char **argv);\n";
    output_string outchan "static int caml_code[] = {\n";
    Symtable.init();
    clear_crc_interfaces ();
    let currpos = ref 0 in
    let output_fun code =
      output_code_string outchan code;
      currpos := !currpos + Bytes.length code
    and currpos_fun () = !currpos in
    List.iter (link_file ppf output_fun currpos_fun) tolink;
    (* The final STOP instruction *)
    Printf.fprintf outchan "\n0x%x};\n\n" Opcodes.opSTOP;
    (* The table of global data *)
    output_string outchan "static char caml_data[] = {\n";
    output_data_string outchan
      (Marshal.to_string (Symtable.initial_global_table()) []);
    output_string outchan "\n};\n\n";
    (* The sections *)
    let sections =
      [ "SYMB", Symtable.data_global_map();
        "PRIM", Obj.repr(Symtable.data_primitive_names());
        "CRCS", Obj.repr(extract_crc_interfaces()) ] in
    output_string outchan "static char caml_sections[] = {\n";
    output_data_string outchan
      (Marshal.to_string sections []);
    output_string outchan "\n};\n\n";
    (* The table of primitives *)
    Symtable.output_primitive_table outchan;
    (* The entry point *)
    output_string outchan "\
\nvoid caml_startup(char ** argv)\
\n{\
\n  caml_startup_code(caml_code, sizeof(caml_code),\
\n                    caml_data, sizeof(caml_data),\
\n                    caml_sections, sizeof(caml_sections),\
\n                    argv);\
\n}\
\n#ifdef __cplusplus\
\n}\
\n#endif\n";
    close_out outchan
  with x ->
    close_out outchan;
    remove_file outfile;
    raise x
  end;
  if !Clflags.debug then
    output_cds_file ((Filename.chop_extension outfile) ^ ".cds")

(* Build a custom runtime *)

let build_custom_runtime prim_name exec_name =
  let runtime_lib = "-lcamlrun" ^ !Clflags.runtime_variant in
  Ccomp.call_linker Ccomp.Exe exec_name
    ([prim_name] @ List.rev !Clflags.ccobjs @ [runtime_lib])
    (Clflags.std_include_flag "-I" ^ " " ^ Config.bytecomp_c_libraries)

let append_bytecode_and_cleanup bytecode_name exec_name prim_name =
  let oc = open_out_gen [Open_wronly; Open_append; Open_binary] 0 exec_name in
  let ic = open_in_bin bytecode_name in
  copy_file ic oc;
  close_in ic;
  close_out oc;
  remove_file bytecode_name;
  remove_file prim_name

(* Fix the name of the output file, if the C compiler changes it behind
   our back. *)

let fix_exec_name name =
  match Sys.os_type with
    "Win32" | "Cygwin" ->
      if String.contains name '.' then name else name ^ ".exe"
  | _ -> name

(* Main entry point (build a custom runtime if needed) *)

let link ppf phase objfiles output_name =
  let tolink =
    if !Clflags.nopervasives || !Clflags.output_c_object then []
    else scan_file phase "std_exit.cmo" []
  in
  let tolink = List.fold_right (scan_file phase) objfiles tolink in
  let tolink =
    if !Clflags.nopervasives || !Clflags.no_std_include then tolink
    else scan_file phase "stdlib.cma" tolink
  in
  scan_path phase (Objects tolink);
  let tolink = complete_link_actions phase tolink in
  Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs; (* put user's libs last *)
  Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
                                                   (* put user's opts first *)
  Clflags.dllibs := !lib_dllibs @ !Clflags.dllibs; (* put user's DLLs first *)
  if not !Clflags.custom_runtime then
    link_bytecode ppf tolink output_name true
  else if not !Clflags.output_c_object then begin
    let bytecode_name = Filename.temp_file "camlcode" "" in
    let prim_name = Filename.temp_file "camlprim" ".c" in
    try
      link_bytecode ppf tolink bytecode_name false;
      let poc = open_out prim_name in
      output_string poc "\
        #ifdef __cplusplus\n\
        extern \"C\" {\n\
        #endif\n\
        #ifdef _WIN64\n\
        #ifdef __MINGW32__\n\
        typedef long long value;\n\
        #else\n\
        typedef __int64 value;\n\
        #endif\n\
        #else\n\
        typedef long value;\n\
        #endif\n";
      Symtable.output_primitive_table poc;
      output_string poc "\
        #ifdef __cplusplus\n\
        }\n\
        #endif\n";
      close_out poc;
      let exec_name = fix_exec_name output_name in
      if not (build_custom_runtime prim_name exec_name)
      then raise(Error Custom_runtime);
      if !Clflags.make_runtime
      then (remove_file bytecode_name; remove_file prim_name)
      else append_bytecode_and_cleanup bytecode_name exec_name prim_name
    with x ->
      remove_file bytecode_name;
      remove_file prim_name;
      raise x
  end else begin
    let basename = Filename.chop_extension output_name in
    let c_file =
      if !Clflags.output_complete_object
      then Filename.temp_file "camlobj" ".c"
      else basename ^ ".c"
    and obj_file =
      if !Clflags.output_complete_object
      then Filename.temp_file "camlobj" Config.ext_obj
      else basename ^ Config.ext_obj
    in
    if Sys.file_exists c_file then raise(Error(File_exists c_file));
    let temps = ref [] in
    try
      link_bytecode_as_c ppf tolink c_file;
      if not (Filename.check_suffix output_name ".c") then begin
        temps := c_file :: !temps;
        if Ccomp.compile_file c_file <> 0 then
          raise(Error Custom_runtime);
        if not (Filename.check_suffix output_name Config.ext_obj) ||
           !Clflags.output_complete_object then begin
          temps := obj_file :: !temps;
          let mode, c_libs =
            if Filename.check_suffix output_name Config.ext_obj
            then Ccomp.Partial, ""
            else Ccomp.MainDll, Config.bytecomp_c_libraries
          in
          if not (
            let runtime_lib = "-lcamlrun" ^ !Clflags.runtime_variant in
            Ccomp.call_linker mode output_name
              ([obj_file] @ List.rev !Clflags.ccobjs @ [runtime_lib])
              c_libs
           ) then raise (Error Custom_runtime);
        end
      end;
      List.iter remove_file !temps
    with x ->
      List.iter remove_file !temps;
      raise x
  end

let load_compunit ppf phase ic filename compunit =
  Printf.eprintf "loading %s\n%!" filename;
  check_consistency ppf filename compunit;
  seek_in ic compunit.cu_pos;
  let code_size = compunit.cu_codesize + 8 in
  let code = Meta.static_alloc code_size in
  unsafe_really_input ic code 0 compunit.cu_codesize;
  Bytes.unsafe_set code compunit.cu_codesize (Char.chr Opcodes.opRETURN);
  String.unsafe_blit "\000\000\000\001\000\000\000" 0
                     code (compunit.cu_codesize + 1) 7;
  let initial_symtable = Symtable.current_state () in
  Symtable.patch_object phase code compunit.cu_reloc;
  Symtable.update_global_table ();
  let events =
    if compunit.cu_debug = 0 then [| |]
    else begin
      seek_in ic compunit.cu_debug;
      [| input_value ic |]
    end in
  Meta.add_debug_info code code_size events;
  begin try
    ignore ((Meta.reify_bytecode code code_size) ());
  with exn ->
    Symtable.restore_state initial_symtable;
    raise exn
  end

let load_object ppf phase filename compunit =
  let inchan = open_in_bin filename in
  try
    load_compunit ppf phase inchan filename compunit;
    close_in inchan
  with
  | Symtable.Error msg ->
      close_in inchan; raise (Error (Symbol_error (filename, msg)))
  | x ->
      close_in inchan; raise x

let load_archive ppf phase filename units_required =
  let inchan = open_in_bin filename in
  try
    List.iter
      (fun cu ->
         let name = filename ^ "(" ^ cu.cu_name ^ ")" in
         try
           load_compunit ppf phase inchan name cu
         with Symtable.Error msg ->
           raise(Error(Symbol_error(name, msg))))
      units_required;
    close_in inchan
  with x -> close_in inchan; raise x

let load_file ppf = function
  | Link_object (filename, unit, phase) ->
      load_object ppf phase filename unit
  | Link_archive (filename, units, phase) ->
      load_archive ppf phase filename units

let load_bytecode ppf tolink =
  List.iter (load_file ppf) tolink

let load ppf phase obj_names =
  let tolink = List.fold_right (scan_file phase) obj_names [] in
  let tolink =
    if !Clflags.nopervasives || !Clflags.no_std_include then tolink
    else scan_file phase "stdlib.cma" tolink
  in
  scan_path phase (Objects tolink);
  let tolink = complete_link_actions phase tolink in
  (*
  (* Initialize the DLL machinery *)
  let sharedobjs = List.map Dll.extract_dll_name !Clflags.dllibs in
  Dll.init_compile !Clflags.no_std_include;
  Dll.add_path !load_path;
  begin try Dll.open_dlls Dll.For_checking sharedobjs
  with Failure reason -> raise(Error(Cannot_open_dll reason))
  end;
  *)
  load_bytecode ppf tolink

let load_deps ppf phase obj_names reloc =
  (*
  let tolink =
    if !Clflags.nopervasives then []
    else scan_file phase "std_exit.cmo" []
  in
  *)
  let tolink = List.fold_right (scan_file phase) obj_names [] in
  let tolink =
    if !Clflags.nopervasives || !Clflags.no_std_include then tolink
    else scan_file phase "stdlib.cma" tolink
  in
  scan_path phase (Reloc_info reloc);
  let tolink = complete_link_actions phase tolink in
  (*
  (* Initialize the DLL machinery *)
  let sharedobjs = List.map Dll.extract_dll_name !Clflags.dllibs in
  Dll.init_compile !Clflags.no_std_include;
  Dll.add_path !load_path;
  begin try Dll.open_dlls Dll.For_checking sharedobjs
  with Failure reason -> raise(Error(Cannot_open_dll reason))
  end;
  *)
  load_bytecode ppf tolink

(* Error report *)

open Format

let report_error ppf = function
  | File_not_found name ->
      fprintf ppf "Cannot find file %a" Location.print_filename name
  | Not_an_object_file name ->
      fprintf ppf "The file %a is not a bytecode object file"
        Location.print_filename name
  | Wrong_object_name name ->
      fprintf ppf "The output file %s has the wrong name. The extension implies\
                  \ an object file but the link step was requested" name
  | Symbol_error(name, err) ->
      fprintf ppf "Error while linking %a:@ %a" Location.print_filename name
      Symtable.report_error err
  | Inconsistent_import(intf, file1, file2) ->
      fprintf ppf
        "@[<hov>Files %a@ and %a@ \
                 make inconsistent assumptions over interface %s@]"
        Location.print_filename file1
        Location.print_filename file2
        intf
  | Custom_runtime ->
      fprintf ppf "Error while building custom runtime system"
  | File_exists file ->
      fprintf ppf "Cannot overwrite existing file %a"
        Location.print_filename file
  | Cannot_open_dll file ->
      fprintf ppf "Error on dynamically loaded library: %a"
        Location.print_filename file
  | Not_compatible_32 ->
      fprintf ppf "Generated bytecode executable cannot be run\
                  \ on a 32-bit platform"

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

let reset () =
  lib_ccobjs := [];
  lib_ccopts := [];
  lib_dllibs := [];
  globals := Ident.empty;
  Consistbl.clear crc_interfaces;
  implementations_defined := [];
  debug_info := [];
  output_code_string_counter := 0
