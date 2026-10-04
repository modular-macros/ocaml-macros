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
  | Static_runner_not_found of string
  | Static_link_failed of string
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

(* The objects that the runner loads *)
let static_objfiles ~static_cmo =
  (* [Dynlink] resolves a relative name against the current directory. *)
  let static_cmo =
    if Filename.is_relative static_cmo
    then Filename.concat (Sys.getcwd ()) static_cmo
    else static_cmo
  in
  [ static_cmo ]

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
         let objs = static_objfiles ~static_cmo:(static_prefix ^ ".cmo") in
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
  if exports then emit_macros_object i typed;
  if Env.get_unit_has_top_level_splice ()
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
  | Static_runner_not_found file ->
      Location.errorf ~loc
        "Cannot find %a,@ the static-program runner built with the \
         compiler."
        Style.inline_code file
  | Static_link_failed msg ->
      Location.errorf ~loc
        "Linking this unit's compile-time part failed:@ %s" msg
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
