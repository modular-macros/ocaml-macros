let failf fmt = Format.kasprintf failwith fmt

(* Mostly adapted from dynlink_common.ml  / otherlibs/dynlink/byte/dynlink.ml *)

let really_input_bigarray ic ar st n =
  match In_channel.really_input_bigarray ic ar st n with
    | None -> raise End_of_file
    | Some () -> ()

let run_aux code (compunit : Cmo_format.compilation_unit) file_digest ~priv old_state : Obj.t =
  begin try
    Symtable.patch_object code compunit.cu_reloc;
    Symtable.check_global_initialized compunit.cu_reloc;
    Symtable.update_global_table ()
  with Symtable.Error (Symtable.Undefined_global g) ->
        failf "Undefined global: %s" (Symtable.Global.name g)
      | Symtable.Error (Symtable.Unavailable_primitive s) ->
         failf "Unavailable primitive: %s" s
      | Symtable.Error (Symtable.Uninitialized_global g) ->
         failf "Uninitialized global %s" (Symtable.Global.name g)
      | Symtable.Error (Symtable.Wrong_vm _) ->
         failf "wrong vm"
  end;
  (* PR#5215: identify this code fragment by
     digest of file contents + unit name.
     Unit name is needed for .cma files, which produce several code
     fragments. *)
  let unit_name = Symtable.Compunit.name compunit.cu_name in
  let digest = Digest.string (file_digest ^ unit_name) in
  if priv then Symtable.hide_additions old_state;
  let _, clos = Meta.reify_bytecode code [| |] (Some digest) in
  try  ((clos ()) : Obj.t);
  with exn ->
    failf "Library's module initializers failed (%s)" (Printexc.to_string exn)
  

let run (ic, file_digest) ~unit_header ~priv =
  let old_state = Symtable.current_state () in
  let compunit : Cmo_format.compilation_unit = unit_header in
  seek_in ic compunit.cu_pos;
  let code =
    Bigarray.Array1.create Bigarray.Char Bigarray.c_layout
      compunit.cu_codesize
  in
  really_input_bigarray ic code 0 compunit.cu_codesize;
  run_aux code unit_header file_digest ~priv old_state

let load_file ~filename:file_name =
  let ic =
    try open_in_bin file_name
    with exc -> failf "cannot open dynamic library (%s)" (Printexc.to_string exc)
  in
  try
    let file_digest = Digest.channel ic (-1) in
    seek_in ic 0;
    let buffer =
      try really_input_string ic (String.length Config.cmo_magic_number)
      with End_of_file -> failf "not a bytecode file %s" file_name
    in
    let handle = ic, file_digest in
    if buffer = Config.cmo_magic_number then
      let compunit_pos = input_binary_int ic in  (* Go to descriptor *)
      seek_in ic compunit_pos;
      let cu = (input_value ic : Cmo_format.compilation_unit) in
      handle, [cu]
    else if buffer = Config.cma_magic_number then begin
      let toc_pos = input_binary_int ic in  (* Go to table of contents *)
      seek_in ic toc_pos;
      let lib = (input_value ic : Cmo_format.library) in
      (* Symtable.open_dlls lib.lib_dllibs; *)
      handle, lib.lib_units
      end
    else failf "not a bytecode file %s" file_name
  with
  | exc ->
    close_in_noerr ic;
    failf "cannot open dynamic library (%s)" (Printexc.to_string exc)

let dll_filename fname =
  if Filename.is_implicit fname then Filename.concat (Sys.getcwd ()) fname
  else fname

let load priv filename =
  (* init (); *)
  let filename = dll_filename filename in
  let (ic, file_digest), units = load_file ~filename in
    List.iter (fun unit_header -> ignore (run (ic, file_digest) ~unit_header ~priv)) units

let traverse_and_load_cu_deps (cu_reloc:(Cmo_format.reloc_info * int) list) =
  List.iter
  (function
    | (Cmo_format.Reloc_getcompunit (Compunit s as g), _)
      when not (Symtable.is_global_defined (Glob_compunit g)) ->
       load false (Load_path.find_normalized (s ^ ".cmo"))
    | _ -> ())
  cu_reloc

let run_lambda ppf slam =
  let initial_symtable = Symtable.current_state() in
  ignore (Symtable.init_toplevel ());

  (* TODO: this is a hack to ensure that CamlinternalQuote is in the symbol table *)
  let module X = struct module type t = sig end end in 
  let _ = ignore (module CamlinternalQuote : X.t) in
  let _ = ignore (module Callback : X.t) in
  (* /TODO *)

  let init_code, _ = Bytegen.compile_phrase slam in

  let code, reloc, events = Emitcode.to_memory init_code in
  traverse_and_load_cu_deps reloc;
  begin try
    Symtable.patch_object code reloc;
    Symtable.check_global_initialized reloc;
    Symtable.update_global_table ();
  with Symtable.Error error ->
    failf "Compile-time evaluation failed: %a"
      Symtable.report_error error;
  end;
  let splices =
    let _, closure = Meta.reify_bytecode code [| events |] None in
    try (Obj.obj (closure ()) : Lambda.lambda array)
    with exn -> failf "Compile-time evaluation failed: %s" (Printexc.to_string exn)
  in
  Symtable.reset ();
  Bytelink.reset ();
  let n = Array.length splices in
  let expected_splices = Env.get_tlsplice_count () in
  if n <> expected_splices then
    failf "Expected splice count: %d, actual splice count: %d" n expected_splices;
  if !Clflags.dump_lambda then
    for i = 0 to pred n do
      Format.fprintf ppf "Runstatic splice #%d:@[%a@]@."
        (i + 1) Printlambda.lambda splices.(i);
    done;
  Symtable.restore_state initial_symtable;
  splices
