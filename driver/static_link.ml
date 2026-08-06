(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
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
  | Static_runner_not_found of string
  | Static_link_failed of string
  | Macro_object_not_found of { file : string; unit_name : string }
  | Static_program_failed of { base : string; status : int }
  | Static_program_no_object of { base : string; file : string }
  | Static_program_reported of string

exception Error of Location.t * error

type layout =
  | Build_tree of string
  | Installed

let layout = lazy begin
  let dir = Filename.dirname Sys.executable_name in
  let probe = Filename.concat "compilerlibs" "ocamlcommon.cma" in
  if Sys.file_exists (Filename.concat dir probe)
  then Build_tree dir
  else Installed
end

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

let read_compunit ~loc filename =
  let ic = open_in_bin filename in
  Misc.try_finally ~always:(fun () -> close_in ic)
    (fun () ->
       match
         let magic = String.length Config.cmo_magic_number in
         if really_input_string ic magic <> Config.cmo_magic_number then
           raise (Error (loc, Not_a_bytecode_object filename));
         seek_in ic (input_binary_int ic);
         (input_value ic : Cmo_format.compilation_unit)
       with
       | cu -> cu
       | exception (End_of_file | Failure _ | Sys_error _) ->
           raise (Error (loc, Not_a_bytecode_object filename)))

let macro_units_of compunit =
  List.filter_map
    (function
      | (Cmo_format.Reloc_getcompunit (Cmo_format.Compunit name), _)
        when Filename.check_suffix name "$macros" -> Some name
      | _ -> None)
    compunit.Cmo_format.cu_reloc

let dedup l =
  let seen = Hashtbl.create 8 in
  List.filter
    (fun x ->
       if Hashtbl.mem seen x then false
       else (Hashtbl.add seen x (); true))
    l

let macro_object_closure ~loc roots =
  let seen = Hashtbl.create 8 and order = ref [] in
  let manifest = ref [] in
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
          let cu = read_compunit ~loc path in
          manifest := cu.Cmo_format.cu_static_archives @ !manifest;
          List.iter visit (macro_units_of cu);
          order := path :: !order
    end
  in
  List.iter visit roots;
  (List.rev !order, !manifest)

let static_objfiles ~loc ~static_cmo =
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
  let macro_objects, manifest =
    macro_object_closure ~loc
      (macro_units_of (read_compunit ~loc static_cmo))
  in
  let manifest_archives =
    List.filter_map
      (fun f ->
         if Filename.is_implicit f then
           match Load_path.find f with
           | file when not (List.mem file static_use) -> Some file
           | _ | exception Not_found -> None
         else if List.mem f static_use then None
         else Some f)
      (dedup manifest)
  in
  static_use
  @ manifest_archives
  @ macro_objects
  @ [ static_cmo ]

let run_static_program ~loc base ~objs_file ~emitted =
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
             match (input_value ic : Location.t * string) with
             | reported -> Some reported
             | exception _ -> None )
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
      if not (Sys.file_exists emitted) then
        raise (Error (loc, Static_program_no_object { base; file = emitted }))
  | None, n ->
      raise (Error (loc, Static_program_failed { base; status = n }))

let should_emit_macros_object ~exports ~static_will_emit =
  match !Clflags.macros_object with
  | Clflags.Macros_object_none -> false
  | Clflags.Macros_object_auto -> exports && not static_will_emit
  | Clflags.Macros_object_always -> not static_will_emit

let emit_macros_object i Typedtree.{structure; coercion; _} =
  let source_file = Unit_info.source_file i.target in
  let prog =
    Translmod.transl_macros_object (Unit_info.modname i.target)
      (structure, coercion)
  in
  Emit_cmo.to_cmo ~source_file
    ~prefix:(Unit_info.prefix i.target ^ "$macros")
    ~imports:(Env.imports ())
    ~flags:(Clflags.emitter_flags ())
    ~primitives:!Translmod.primitive_declarations
    ~required_globals:prog.Lambda.required_globals
    (Simplif.simplify_lambda prog.Lambda.code)

let build_static_program ~native i Typedtree.{structure; coercion; _} =
  let source_file = Unit_info.source_file i.target in
  let loc = Location.in_file source_file in
  let prefix = Unit_info.prefix i.target in
  let static_prefix = prefix ^ "$static" in
  let prog =
    Profile.(record transl)
      (fun () ->
         Translmod.transl_static_program ~native
           (Unit_info.modname i.target) (structure, coercion)
           ~source_file ~prefix)
      ()
  in
  let lambda =
    prog.Lambda.code
    |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.lambda
    |> Simplif.simplify_lambda
    |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.lambda
  in
  Profile.(record ~accumulate:true generate)
    (fun () ->
       Emit_cmo.to_cmo ~source_file ~prefix:static_prefix
         ~imports:(Env.imports ())
         ~flags:(Clflags.emitter_flags ())
         ~primitives:!Translmod.primitive_declarations
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
       let emitted =
         Unit_info.Artifact.filename
           (if native then Unit_info.cmx i.target else Unit_info.cmo i.target)
       in
       remove_file emitted;
       run_static_program ~loc static_prefix ~objs_file ~emitted;
       remove_file objs_file;
       remove_file (static_prefix ^ ".cmo"))
    ()

let implementation ~native i typed ~fallback =
  let str_exports = Translmod.exports_macros typed.Typedtree.structure in
  let sig_exports =
    Mtype.has_macro_components ~expand_aliases:false
      typed.Typedtree.structure.str_final_env
      (Types.Mty_signature typed.Typedtree.signature)
  in
  let exports = str_exports || sig_exports in
  let static_will_emit =
    Translmod.quoted_builders () && Env.get_tlsplice_count () <> 0
    && str_exports
  in
  if should_emit_macros_object ~exports ~static_will_emit then
    emit_macros_object i typed;
  if Env.get_tlsplice_count () <> 0
     || Translmod.has_template_applications typed.Typedtree.structure
  then build_static_program ~native i typed
  else fallback ()

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
  | Static_runner_not_found file ->
      Location.errorf ~loc
        "Cannot find %a,@ the static-program runner built with the \
         compiler."
        Style.inline_code file
  | Static_link_failed msg ->
      Location.errorf ~loc
        "Linking this unit's compile-time part failed:@ %s\
       @ This unit has a compile-time part, so a definition naming\
       @ another unit is made in both worlds, and both must link.\
       @ An extension rebinding --\
       @ %a --\
       @ is the usual way to reach this error: its constructor is usable\
       @ at either level, so the compile-time world needs that unit too.\
       @ Make it available there with %a,\
       @ and %a on its directory."
        msg
        Style.inline_code "exception E = Other_unit.E"
        Style.inline_code "-static-use"
        Style.inline_code "-I-static"
  | Macro_object_not_found { file; unit_name } ->
      Location.errorf ~loc
        "Cannot find %a, which holds the macros of %a."
        Style.inline_code file Style.inline_code unit_name
  | Static_program_failed { base; status } ->
      Location.errorf ~loc
        "Compile-time evaluation failed:@ the static program %s.@ \
         Its object %a and object list %a@ were kept for inspection."
        (describe_status status)
        Style.inline_code (base ^ ".cmo")
        Style.inline_code (base ^ ".objs")
  | Static_program_no_object { base; file } ->
      Location.errorf ~loc
        "Compile-time evaluation ended without emitting %a.\
       @ The static program exited with status 0 before writing it;\
       @ a macro calling %a would do this.\
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
