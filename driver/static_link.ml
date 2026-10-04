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

open Misc
open Compile_common

type error =
  | Not_a_bytecode_object of string
  | Static_archive_not_found of string
  | Static_archives_inconsistent of Linkdeps.error
  | Static_runner_not_found of string
  | Static_link_failed of string
  | Macro_object_not_found of { file : string; unit_name : string }
  | Static_program_failed of { base : string; status : int }
  | Static_program_no_holes of { base : string; file : string }
  | Static_program_reported of string

exception Error of Location.t * error

(* The compiler runs from its build tree and from an installation,
   which put the runner and the runtime in different places.  Both are
   found from the place of the compiler. *)
type layout =
  | Build_tree of string (* the directory holding the compiler *)
  | Installed

let layout = lazy begin
  let dir = Filename.dirname Sys.executable_name in
  let probe = Filename.concat "compilerlibs" "ocamlcommon.cma" in
  if Sys.file_exists (Filename.concat dir probe)
  then Build_tree dir
  else Installed
end

(* The runner is run under an explicit runtime: in a build tree its
   header names the runtime of an installation that need not exist. *)
let ocamlrun () =
  match Lazy.force layout with
  | Build_tree root ->
      Filename.concat root (Filename.concat "runtime" "ocamlrun")
  | Installed ->
      Filename.concat (Filename.dirname Sys.executable_name) "ocamlrun"

let static_runner () =
  match Lazy.force layout with
  | Build_tree root -> Filename.concat root "staticrun"
  | Installed ->
      Filename.concat (Filename.dirname Sys.executable_name) "staticrun"

(* The compile-time parts that a static program needs are read from
   relocation tables: the compile-time part of a unit may itself use
   that of another.  The run-time objects are not loaded. *)
let read_compunits ~loc filename =
  let ic = open_in_bin filename in
  Misc.try_finally ~always:(fun () -> close_in ic)
    (fun () ->
       (* A file that cannot be read as an object, such as an
          interrupted build leaves, is reported as not being one. *)
       match
         let magic =
           really_input_string ic (String.length Config.cmo_magic_number)
         in
         seek_in ic (input_binary_int ic);
         if magic = Config.cmo_magic_number then
           [ (input_value ic : Cmo_format.compilation_unit) ]
         else if magic = Config.cma_magic_number then
           (input_value ic : Cmo_format.library).lib_units
         else raise (Error (loc, Not_a_bytecode_object filename))
       with
       | cus -> cus
       | exception (End_of_file | Failure _ | Sys_error _) ->
           raise (Error (loc, Not_a_bytecode_object filename)))

let read_compunit ~loc filename =
  match read_compunits ~loc filename with
  | [ cu ] -> cu
  | _ -> raise (Error (loc, Not_a_bytecode_object filename))

let used_units compunit =
  Symtable.required_compunits compunit.Cmo_format.cu_reloc
  @ compunit.Cmo_format.cu_required_compunits
  |> List.map (fun (Cmo_format.Compunit name) -> name)

let macro_units_of compunit =
  List.filter
    (fun name -> Filename.check_suffix name "$macros")
    (used_units compunit)

(* The files given to -static-use are linked in the order given, as the
   files of a program are, and are checked as those are: a file that
   uses a unit of a later file, and two files that define the same unit,
   are errors.  A unit that no file defines may be one that the runner
   is linked with: its absence is left for the link to report. *)
let check_link_order ~loc files =
  let ldeps = Linkdeps.create ~complete:false in
  List.iter
    (fun filename ->
       List.iter
         (fun cu ->
            let Cmo_format.Compunit compunit = cu.Cmo_format.cu_name in
            Linkdeps.add ldeps ~filename ~compunit ~provides:[compunit]
              ~requires:(used_units cu))
         (List.rev (read_compunits ~loc filename)))
    (List.rev files);
  Option.iter
    (fun err -> raise (Error (loc, Static_archives_inconsistent err)))
    (Linkdeps.check ldeps)

(* The objects that hold the compile-time definitions of [roots] and of
   the units that they depend on, in link order. *)
let macro_object_closure ~loc roots =
  let seen = Hashtbl.create 8 in
  let objects = ref [] in
  let rec visit name =
    if not (Hashtbl.mem seen name) then begin
      Hashtbl.add seen name ();
      let file = Unit_info.normalize name ^ ".cmo" in
      match Load_path.find_normalized file with
      | exception Not_found ->
          raise (Error (loc, Macro_object_not_found
                               { file;
                                 unit_name =
                                   Filename.chop_suffix name "$macros" }))
      | path ->
          List.iter visit (macro_units_of (read_compunit ~loc path));
          objects := path :: !objects
    end
  in
  List.iter visit roots;
  List.rev !objects

(* What the runner loads on top of the libraries that it is linked
   with: the files given to -static-use, the objects that hold the
   compile-time definitions of other units, and the static program
   itself. *)
let static_objfiles ~loc ~static_cmo =
  (* [Dynlink] resolves a relative name against the current directory. *)
  let static_cmo =
    if Filename.is_relative static_cmo
    then Filename.concat (Sys.getcwd ()) static_cmo
    else static_cmo
  in
  let resolve f =
    if Filename.is_implicit f then
      match Load_path.find f with
      | file -> file
      | exception Not_found ->
          raise (Error (loc, Static_archive_not_found f))
    else f
  in
  let static_use = List.rev_map resolve !Clflags.static_use in
  check_link_order ~loc static_use;
  let macro_objects =
    macro_object_closure ~loc
      (macro_units_of (read_compunit ~loc static_cmo))
  in
  static_use @ macro_objects @ [ static_cmo ]

(* The failures of the run are read from files.  A failure to load the
   static program is text in [base ^ ".linkerr"], which the runner
   writes.  A failure that the static program reports, a located error,
   is in [base ^ ".err"]; see [Static_report]. *)
let run_static_program ~loc base ~objs_file ~holes_file =
  let err_file = base ^ ".err" in
  let link_file = base ^ ".linkerr" in
  remove_file err_file;
  remove_file link_file;
  let runner = static_runner () in
  if not (Sys.file_exists runner) then
    raise (Error (loc, Static_runner_not_found runner));
  let status =
    Sys.command
      (Filename.quote_command (ocamlrun ()) [runner; base; objs_file])
  in
  if Sys.file_exists link_file then begin
    let msg =
      let ic = open_in_bin link_file in
      Misc.try_finally ~always:(fun () -> close_in ic)
        (fun () -> really_input_string ic (in_channel_length ic))
    in
    remove_file link_file;
    raise (Error (loc, Static_link_failed msg))
  end;
  let reported =
    if not (Sys.file_exists err_file) then None
    else begin
      let contents =
        let ic = open_in_bin err_file in
        Misc.try_finally ~always:(fun () -> close_in ic)
          (fun () ->
             match (input_value ic : Static_report.failure) with
             | reported -> Some reported
             | exception _ -> None)
      in
      remove_file err_file;
      contents
    end
  in
  match reported, status with
  | Some (rloc, msg), _ ->
      remove_file (base ^ ".cmo");
      remove_file objs_file;
      raise (Error (rloc, Static_program_reported msg))
  | None, 0 ->
      (* The holes, not the status, are the criterion of success:
         compile-time code that calls [exit 0] ends the static program
         before it writes them. *)
      if not (Sys.file_exists holes_file) then
        raise (Error (loc, Static_program_no_holes { base; file = holes_file }))
  | None, n ->
      raise (Error (loc, Static_program_failed { base; status = n }))

let should_emit_macros_object ~exports =
  match !Clflags.macros_object with
  | Clflags.Macros_object_none -> false
  | Clflags.Macros_object_auto -> exports
  | Clflags.Macros_object_always -> true

(* The static program is bytecode whichever compiler builds it, and is
   simplified as bytecode. *)
let simplify_compile_time lam =
  Misc.protect_refs [ Misc.R (Clflags.native_code, false) ]
    (fun () -> Simplif.simplify_lambda lam)

let emit_macros_object i Typedtree.{structure; coercion; _} =
  let source_file = Unit_info.source_file i.target in
  let prog =
    Translmod.transl_macros_object (Unit_info.modname i.target)
      (structure, coercion)
  in
  Emit_cmo.to_cmo ~source_file
    ~prefix:(Unit_info.prefix i.target ^ "$macros")
    ~required_globals:prog.Lambda.required_globals
    (simplify_compile_time prog.Lambda.code)

(* The run-time term of the unit, its holes filled by its static
   program.  The static program is printed under -dstatic, and the term
   is left to the caller, which compiles it as that of any unit. *)
let build_static_program ~native i Typedtree.{structure; coercion; _} =
  let source_file = Unit_info.source_file i.target in
  let loc = Location.in_file source_file in
  let prefix = Unit_info.prefix i.target in
  let static_prefix = prefix ^ "$static" in
  let prog, run_time =
    Profile.(record transl)
      (fun () ->
         Translmod.transl_static_program ~native
           (Unit_info.modname i.target) (structure, coercion)
           ~source_file ~prefix)
      ()
  in
  let lambda =
    prog.Lambda.code
    |> simplify_compile_time
    |> print_if i.ppf_dump Clflags.dump_static Printlambda.lambda
  in
  let holes_file = static_prefix ^ ".holes" in
  let holes =
    Profile.(record ~accumulate:true generate)
      (fun () ->
         Emit_cmo.to_cmo ~source_file ~prefix:static_prefix
           ~required_globals:prog.Lambda.required_globals lambda;
         let objs =
           static_objfiles ~loc ~static_cmo:(static_prefix ^ ".cmo")
         in
         let objs_file = static_prefix ^ ".objs" in
         let oc = open_out objs_file in
         Misc.try_finally ~always:(fun () -> close_out oc)
           (fun () ->
              List.iter
                (fun f -> output_string oc f; output_char oc '\n')
                objs);
         (* The holes are removed first, so that those of an earlier
            compilation are not taken for them. *)
         remove_file holes_file;
         run_static_program ~loc static_prefix ~objs_file ~holes_file;
         remove_file objs_file;
         remove_file (static_prefix ^ ".cmo");
         let holes =
           In_channel.with_open_bin holes_file
             (fun ic -> (input_value ic : Lambda.lambda array))
         in
         remove_file holes_file;
         holes)
      ()
  in
  { run_time with
    Lambda.code = Translquote.fill_holes run_time.Lambda.code holes }

let implementation ~native i typed ~fallback ~finish =
  (* An alias is not expanded: its components are those of another
     unit. *)
  let exports =
    Translmod.exports_macros typed.Typedtree.structure
    || Mtype.has_macro_components ~expand_aliases:false
         typed.Typedtree.structure.str_final_env
         (Types.Mty_signature typed.Typedtree.signature)
  in
  if should_emit_macros_object ~exports then
    emit_macros_object i typed;
  if Env.get_unit_has_top_level_splice ()
     || Translmod.has_template_applications typed.Typedtree.structure
  then finish (build_static_program ~native i typed)
  else fallback ()

(* Error report *)

(* [Sys.command] runs the runner through a shell, which reports a
   child killed by signal n as the exit status 128+n. *)
let signal_name = function
  | 1 -> Some "SIGHUP"   | 2 -> Some "SIGINT"   | 3 -> Some "SIGQUIT"
  | 4 -> Some "SIGILL"   | 6 -> Some "SIGABRT"  | 7 -> Some "SIGBUS"
  | 8 -> Some "SIGFPE"   | 9 -> Some "SIGKILL"  | 11 -> Some "SIGSEGV"
  | 13 -> Some "SIGPIPE" | 14 -> Some "SIGALRM" | 15 -> Some "SIGTERM"
  | 24 -> Some "SIGXCPU" | 25 -> Some "SIGXFSZ"
  | _ -> None

let describe_status status =
  if status > 128 && status < 128 + 64 then
    let n = status - 128 in
    Printf.sprintf "was killed by signal %i%s" n
      (match signal_name n with None -> "" | Some s -> " (" ^ s ^ ")")
  else Printf.sprintf "exited with code %i" status

let report_error loc = function
  | Not_a_bytecode_object file ->
      Location.errorf ~loc "%a is not a bytecode object file."
        Style.inline_code file
  | Static_archive_not_found f ->
      Location.errorf ~loc
        "Cannot find %a (given to -static-use) in the load path."
        Style.inline_code f
  | Static_archives_inconsistent err ->
      Location.errorf ~loc "%a"
        (Linkdeps.report_error_doc ~print_filename:Location.Doc.filename)
        err
  | Static_runner_not_found file ->
      Location.errorf ~loc
        "Cannot find %a,@ the static-program runner built with the \
         compiler."
        Style.inline_code file
  | Static_link_failed msg ->
      Location.errorf ~loc
        "Linking this unit's compile-time part failed:@ %s\
       @ The implementation of a unit that compile-time code uses\
       @ must be given to %a."
        msg
        Style.inline_code "-static-use"
  | Macro_object_not_found { file; unit_name } ->
      Location.errorf ~loc
        "Cannot find %a, which holds the compile-time definitions of %a."
        Style.inline_code file Style.inline_code unit_name
  | Static_program_failed { base; status } ->
      Location.errorf ~loc
        "Compile-time evaluation failed:@ the static program %s.@ \
         Its object %a and object list %a@ were kept for inspection."
        (describe_status status)
        Style.inline_code (base ^ ".cmo")
        Style.inline_code (base ^ ".objs")
  | Static_program_no_holes { base; file } ->
      Location.errorf ~loc
        "Compile-time evaluation ended before the code of the splices \
         was complete.\
       @ The static program exited with status 0 before writing it \
         to %a;\
       @ compile-time code calling %a would do this.\
       @ Its object %a and object list %a\
       @ were kept for inspection."
        Style.inline_code file
        Style.inline_code "exit"
        Style.inline_code (base ^ ".cmo")
        Style.inline_code (base ^ ".objs")
  | Static_program_reported msg ->
      Location.errorf ~loc "%s" msg

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) -> Some (report_error loc err)
      | _ -> None)
