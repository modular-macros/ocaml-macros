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

(* To assign numbers to globals and primitives *)

open Misc
open Asttypes
open Lambda
open Cmo_format

(* Functions for batch linking *)

type error =
    Undefined_global of string
  | Unavailable_primitive of string
  | Wrong_vm of string
  | Uninitialized_global of string

exception Error of error

(* Tables for numbering objects *)

type 'a numtable =
  { num_cnt: int;               (* The next number *)
    num_tbl: ('a, int) Tbl.t }  (* The table of already numbered objects *)

let empty_numtable = { num_cnt = 0; num_tbl = Tbl.empty }

let find_numtable nt key =
  Tbl.find key nt.num_tbl

let map_keys (f : 'a -> 'c) (tbl : ('a, 'b) Tbl.t) : ('c, 'b) Tbl.t =
  let add_new_key k x tbl =
    Tbl.add (f k) x tbl
  in
  Tbl.fold add_new_key tbl Tbl.empty

let enter_numtable nt key =
  let n = !nt.num_cnt in
  nt := { num_cnt = n + 1; num_tbl = Tbl.add key n !nt.num_tbl };
  n

let incr_numtable nt =
  let n = !nt.num_cnt in
  nt := { num_cnt = n + 1; num_tbl = !nt.num_tbl };
  n

(* Global variables *)

let global_table = ref(empty_numtable : (Types.phase * Ident.t) numtable)
and literal_table = ref([] : (int * structured_constant) list)

let is_global_defined id =
  Tbl.mem id (!global_table).num_tbl

let slot_for_getglobal (phase, id) =
  Printf.eprintf "sfgg %s\n%!" (Ident.name id);
  try
    let n = find_numtable !global_table (phase, id) in
    n
  with Not_found ->
    raise(Error(Undefined_global(Ident.name id)))

let slot_for_setglobal id =
  enter_numtable global_table id

let slot_for_literal cst =
  let n = incr_numtable global_table in
  literal_table := (n, cst) :: !literal_table;
  n

(* The C primitives *)

let c_prim_table = ref(empty_numtable : string numtable)

let set_prim_table name =
  ignore(enter_numtable c_prim_table name)

let num_of_prim name =
  try
    find_numtable !c_prim_table name
  with Not_found ->
    if !Clflags.custom_runtime || Config.host <> Config.target
       || !Clflags.no_check_prims
    then
      enter_numtable c_prim_table name
    else begin
      let symb =
        try Dll.find_primitive name
        with Not_found -> raise(Error(Unavailable_primitive name)) in
      let num = enter_numtable c_prim_table name in
      Dll.synchronize_primitive num symb;
      num
    end

let require_primitive name =
  if name.[0] <> '%' then ignore(num_of_prim name)

let all_primitives () =
  let prim = Array.make !c_prim_table.num_cnt "" in
  Tbl.iter (fun name number -> prim.(number) <- name) !c_prim_table.num_tbl;
  prim

let data_primitive_names () =
  let prim = all_primitives() in
  let b = Buffer.create 512 in
  for i = 0 to Array.length prim - 1 do
    Buffer.add_string b prim.(i); Buffer.add_char b '\000'
  done;
  Buffer.contents b

let output_primitive_names outchan =
  output_string outchan (data_primitive_names())

open Printf

let output_primitive_table outchan =
  let prim = all_primitives() in
  for i = 0 to Array.length prim - 1 do
    fprintf outchan "extern value %s();\n" prim.(i)
  done;
  fprintf outchan "typedef value (*primitive)();\n";
  fprintf outchan "primitive caml_builtin_cprim[] = {\n";
  for i = 0 to Array.length prim - 1 do
    fprintf outchan "  %s,\n" prim.(i)
  done;
  fprintf outchan "  (primitive) 0 };\n";
  fprintf outchan "const char * caml_names_of_builtin_cprim[] = {\n";
  for i = 0 to Array.length prim - 1 do
    fprintf outchan "  \"%s\",\n" prim.(i)
  done;
  fprintf outchan "  (char *) 0 };\n"

(* Initialization for batch linking *)

let init () =
  (* Enter the predefined exceptions *)
  Array.iteri
    (fun i name ->
      let id =
        try List.assoc name Predef.builtin_values
        with Not_found -> fatal_error "Symtable.init" in
      let c = slot_for_setglobal (0, id) in
      let cst = Const_block(Obj.object_tag,
                            [Const_base(Const_string (name, None));
                             Const_base(Const_int (-i-1))
                            ])
      in
      literal_table := (c, cst) :: !literal_table;
      (* Add the lifted version of this literal as a pointer to the unlifted
         version (the only one actually existing in the runtime *)
      let lifted_id = Ident.lift_persistent id in
      global_table := { !global_table with
        num_tbl = Tbl.add (1, lifted_id) c !global_table.num_tbl })
    Runtimedef.builtin_exceptions;
  (* Initialize the known C primitives *)
  if String.length !Clflags.use_prims > 0 then begin
      let ic = open_in !Clflags.use_prims in
      try
        while true do
          set_prim_table (input_line ic)
        done
      with End_of_file -> close_in ic
         | x -> close_in ic; raise x
  end else if String.length !Clflags.use_runtime > 0 then begin
    let primfile = Filename.temp_file "camlprims" "" in
    try
      if Sys.command(Printf.sprintf "%s -p > %s"
                                    !Clflags.use_runtime primfile) <> 0
      then raise(Error(Wrong_vm !Clflags.use_runtime));
      let ic = open_in primfile in
      try
        while true do
          set_prim_table (input_line ic)
        done
      with End_of_file -> close_in ic; remove_file primfile
         | x -> close_in ic; raise x
    with x -> remove_file primfile; raise x
  end else begin
    Array.iter set_prim_table Runtimedef.builtin_primitives
  end

(* Relocate a block of object bytecode *)

(* Must use the unsafe String.set here because the block may be
   a "fake" string as returned by Meta.static_alloc. *)

let gen_patch_int str_set buff pos n =
  str_set buff pos (Char.unsafe_chr n);
  str_set buff (pos + 1) (Char.unsafe_chr (n asr 8));
  str_set buff (pos + 2) (Char.unsafe_chr (n asr 16));
  str_set buff (pos + 3) (Char.unsafe_chr (n asr 24))

let gen_patch_object phase str_set buff patchlist =
  List.iter
    (function
        (Reloc_literal sc, pos) ->
          gen_patch_int str_set buff pos (slot_for_literal sc)
      | (Reloc_getglobal id, pos) ->
          gen_patch_int str_set buff pos (slot_for_getglobal (phase, id))
      | (Reloc_setglobal id, pos) ->
          gen_patch_int str_set buff pos (slot_for_setglobal (phase, id))
      | (Reloc_primitive name, pos) ->
          gen_patch_int str_set buff pos (num_of_prim name))
    patchlist

let patch_object phase = gen_patch_object phase Bytes.unsafe_set
let ls_patch_object phase = gen_patch_object phase LongString.set

(* Translate structured constants *)

let rec transl_const = function
    Const_base(Const_int i) -> Obj.repr i
  | Const_base(Const_char c) -> Obj.repr c
  | Const_base(Const_string (s, _)) -> Obj.repr s
  | Const_base(Const_float f) -> Obj.repr (float_of_string f)
  | Const_base(Const_int32 i) -> Obj.repr i
  | Const_base(Const_int64 i) -> Obj.repr i
  | Const_base(Const_nativeint i) -> Obj.repr i
  | Const_pointer i -> Obj.repr i
  | Const_immstring s -> Obj.repr s
  | Const_block(tag, fields) ->
      let block = Obj.new_block tag (List.length fields) in
      let pos = ref 0 in
      List.iter
        (fun c -> Obj.set_field block !pos (transl_const c); incr pos)
        fields;
      block
  | Const_float_array fields ->
      Obj.repr(Array.of_list(List.map (fun f -> float_of_string f) fields))

(* Build the initial table of globals *)

let initial_global_table () =
  let glob = Array.make !global_table.num_cnt (Obj.repr 0) in
  List.iter
    (fun (slot, cst) -> glob.(slot) <- transl_const cst)
    !literal_table;
  literal_table := [];
  glob

let data_global_map () =
  Obj.repr !global_table

(* Functions for toplevel use *)

(* Update the in-core table of globals *)

let update_global_table () =
  let ng = !global_table.num_cnt in
  Printf.eprintf "ng %d\n%!" ng;
  Printf.eprintf "a\n%!";
  if ng > Array.length(Meta.global_data()) then (Printf.eprintf "realloc\n%!";Meta.realloc_global_data ng);
  Printf.eprintf "b\n%!";
  let glob = Meta.global_data() in
  List.iter
    (fun (slot, cst) ->
      Printf.eprintf "fun %d\n%!" slot;
      glob.(slot) <- transl_const cst)
    !literal_table;
  literal_table := []

(* Recover data for toplevel initialization.  Data can come either from
   executable file (normal case) or from linked-in data (-output-obj). *)

type section_reader = {
  read_string: string -> string;
  read_struct: string -> Obj.t;
  close_reader: unit -> unit
}

let read_sections () =
  try
    let sections = Meta.get_section_table () in
    { read_string =
        (fun name -> (Obj.magic(List.assoc name sections) : string));
      read_struct =
        (fun name -> List.assoc name sections);
      close_reader =
        (fun () -> ()) }
  with Not_found ->
    let ic = open_in_bin Sys.executable_name in
    Bytesections.read_toc ic;
    { read_string = Bytesections.read_section_string ic;
      read_struct = Bytesections.read_section_struct ic;
      close_reader = fun () -> close_in ic }

(* Load the table of globals, all saved globals having phase 0. *)

type saved_global_map = Ident.t numtable

(* "Filter" the global map according to some predicate.
   Used to expunge the global map for the toplevel. *)

let filter_global_map p gmap =
  let newtbl = ref Tbl.empty in
  Tbl.iter
    (fun id num -> if p id then newtbl := Tbl.add id num !newtbl)
    gmap.num_tbl;
  {num_cnt = gmap.num_cnt; num_tbl = !newtbl}

(* Save the table of globals *)

let output_global_map oc =
  output_value oc ({ (!global_table : (Types.phase * Ident.t) numtable) with num_tbl =
    (map_keys (fun (_,id) -> id) (filter_global_map (fun (p,_) -> p = 0) !global_table).num_tbl) }
    : Ident.t numtable)

let saved_to_runtime id_numtable =
  { id_numtable with
      num_tbl = map_keys (fun id -> (0, id)) id_numtable.num_tbl }

let runtime_to_saved numtable =
  { numtable with
      num_tbl = map_keys (fun (_, id) -> id)
        (filter_global_map (fun (p,_) -> p = 0) numtable).num_tbl }

(* Initialize the linker for toplevel use *)

(* Find the value of a global identifier *)

let get_global_position id = slot_for_getglobal id

let init_toplevel () =
  try
    let sect = read_sections () in
    (* Locations of globals *)
    global_table := saved_to_runtime
      (Obj.magic (sect.read_struct "SYMB") : Ident.t numtable);
    (* Primitives *)
    let prims = sect.read_string "PRIM" in
    c_prim_table := empty_numtable;
    let pos = ref 0 in
    while !pos < String.length prims do
      let i = String.index_from prims !pos '\000' in
      set_prim_table (String.sub prims !pos (i - !pos));
      pos := i + 1
    done;
    (* DLL initialization *)
    let dllpath = try sect.read_string "DLPT" with Not_found -> "" in
    Dll.init_toplevel dllpath;
    (* Recover CRC infos for interfaces *)
    let crcintfs =
      try
        (Obj.magic (sect.read_struct "CRCS") : (string * Digest.t option) list)
      with Not_found -> [] in
    (* Done *)
    sect.close_reader();
    (* Add lifted version of the `Toploop` module to phase 1 *)
    let id = Ident.create_persistent "Toploop" in
    let pos = get_global_position (0, id) in
    global_table := { !global_table
      with num_tbl = Tbl.add (1, id) pos !global_table.num_tbl };
    (* Add built-in exceptions to phase 1 *)
    Array.iter
      (fun name ->
        let id =
          try List.assoc name Predef.builtin_values
          with Not_found -> fatal_error "Symtable.init" in
        let lifted_id = Ident.create_persistent ("^" ^ Ident.name id) in
        let pos = get_global_position (0, id) in
        global_table := { !global_table with
          num_tbl = Tbl.add (1, lifted_id) pos !global_table.num_tbl })
      Runtimedef.builtin_exceptions;
    Bytesections.reset ();
    crcintfs
  with Bytesections.Bad_magic_number | Not_found | Failure _ ->
    fatal_error "Toplevel bytecode executable is corrupted"

let get_global_value id =
  (Meta.global_data()).(slot_for_getglobal id)
let assign_global_value id v =
  (Meta.global_data()).(slot_for_getglobal id) <- v

(* Initialize the linker for running static code *)

let init_static () =
  if Sys.backend_type = Sys.Bytecode then
    (begin try
      let sect = read_sections () in
      (* Locations of globals *)
      global_table := saved_to_runtime
        (Obj.magic (sect.read_struct "SYMB") : Ident.t numtable);
      (* Primitives *)
      let prims = sect.read_string "PRIM" in
      c_prim_table := empty_numtable;
      let pos = ref 0 in
      while !pos < String.length prims do
        let i = String.index_from prims !pos '\000' in
        set_prim_table (String.sub prims !pos (i - !pos));
        pos := i + 1
      done;
      (* DLL initialization *)
      let dllpath = try sect.read_string "DLPT" with Not_found -> "" in
      Dll.init_toplevel dllpath;
      let crc =
        try
          (Obj.magic (sect.read_struct "CRCS") : (string * Digest.t option) list)
        with Not_found -> []
      in
      sect.close_reader();
      (* Add built-in exceptions to phase 1 *)
      Array.iter
        (fun name ->
          let id =
            try List.assoc name Predef.builtin_values
            with Not_found -> fatal_error "Symtable.init" in
          let lifted_id = Ident.create_persistent ("^" ^ Ident.name id) in
          let pos = get_global_position (0, id) in
          global_table := { !global_table with
            num_tbl = Tbl.add (1, lifted_id) pos !global_table.num_tbl })
        Runtimedef.builtin_exceptions;
      Bytesections.reset ();
      crc
    with Bytesections.Bad_magic_number | Not_found | Failure _ ->
      fatal_error "Toplevel bytecode executable is corrupted"
    end : (string * Digest.t option) list)
  else if Sys.backend_type = Sys.Native then begin
    init ();
    []
  end
  else assert false

(* Check that all globals referenced in the given patch list
   have been initialized already *)

let check_global_initialized phase patchlist =
  (* First determine the globals we will define *)
  let defined_globals =
    List.fold_left
      (fun accu rel ->
        match rel with
          (Reloc_setglobal id, _pos) -> id :: accu
        | _ -> accu)
      [] patchlist in
  (* Then check that all referenced, not defined globals have a value *)
  let check_reference = function
      (Reloc_getglobal id, _pos) ->
        if not (List.mem id defined_globals)
        && Obj.is_int (get_global_value (phase, id))
        then raise (Error(Uninitialized_global(Ident.name id)))
    | _ -> () in
  List.iter check_reference patchlist

(* Save and restore the current state *)

type global_map = (Types.phase * Ident.t) numtable

let current_state () = !global_table

let restore_state st = global_table := st

let hide_additions st =
  if st.num_cnt > !global_table.num_cnt then
    fatal_error "Symtable.hide_additions";
  global_table :=
    { num_cnt = !global_table.num_cnt;
      num_tbl = st.num_tbl }

(* Error report *)

open Format

let report_error ppf = function
  | Undefined_global s ->
      fprintf ppf "Reference to undefined global `%s'" s
  | Unavailable_primitive s ->
      fprintf ppf "The external function `%s' is not available" s
  | Wrong_vm s ->
      fprintf ppf "Cannot find or execute the runtime system %s" s
  | Uninitialized_global s ->
      fprintf ppf "The value of the global `%s' is not yet computed" s

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

let reset () =
  global_table := empty_numtable;
  literal_table := [];
  c_prim_table := empty_numtable
