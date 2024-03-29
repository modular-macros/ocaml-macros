(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The batch compiler *)

open Misc
open Format
open Typedtree
open Compenv
open Asttypes

(* Compile a .mli file *)

(* Keep in sync with the copy in optcompile.ml *)

let tool_name = "ocamlc"

let interface ppf sourcefile outputprefix =
  Compmisc.init_path false;
  let modulename = module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name modulename;
  let initial_env = Compmisc.initial_env () in
  let ast = Pparse.parse_interface ~tool_name ppf sourcefile in

  if !Clflags.dump_parsetree then fprintf ppf "%a@." Printast.interface ast;
  if !Clflags.dump_source then fprintf ppf "%a@." Pprintast.signature ast;
  let tsg = Typemod.type_interface sourcefile initial_env ast in
  if !Clflags.dump_typedtree then fprintf ppf "%a@." Printtyped.interface tsg;
  let sg = tsg.sig_type in
  if !Clflags.print_types then
    Printtyp.wrap_printing_env initial_env (fun () ->
        fprintf std_formatter "%a@."
          Printtyp.signature (Typemod.simplify_signature sg));
  ignore (Includemod.signatures initial_env sg sg);
  Typecore.force_delayed_checks ();
  Warnings.check_fatal ();
  if not !Clflags.print_types then begin
    let deprecated = Builtin_attributes.deprecated_of_sig ast in
    let sg =
      Env.save_signature ~deprecated sg modulename (outputprefix ^ ".cmi")
    in
    Typemod.save_signature modulename tsg outputprefix sourcefile
      initial_env sg ;
  end

(* Compile a .ml file *)

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let (++) x f = f x

let implementation ppf sourcefile outputprefix =
  Compmisc.init_path false;
  let modulename = module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name modulename;
  let env = Compmisc.initial_env() in
  try
    let (typedtree, coercion) =
      Pparse.parse_implementation ~tool_name ppf sourcefile
      ++ print_if ppf Clflags.dump_parsetree Printast.implementation
      ++ print_if ppf Clflags.dump_source Pprintast.structure
      ++ Timings.(time (Typing sourcefile))
          (Typemod.type_implementation sourcefile outputprefix modulename env)
      ++ print_if ppf Clflags.dump_typedtree
        Printtyped.implementation_with_coercion
   in
    if !Clflags.print_types then begin
      Warnings.check_fatal ();
      Stypes.dump (Some (outputprefix ^ ".annot"))
    end else begin
      (* Run static code *)
      let s_prog =
        Translmod.transl_implementation modulename Static (typedtree, coercion)
      in
      let stat_lam =
        print_if ppf Clflags.dump_rawlambda Printlambda.lambda @@
        s_prog.Lambda.code
      in
      let sstat_lam =
        print_if ppf Clflags.dump_lambda Printlambda.lambda @@
        Simplif.simplify_lambda sourcefile stat_lam in
      let w_stat_lam =
        if Sys.backend_type = Sys.Native then
          Translmod.wrap_marshal sstat_lam
        else sstat_lam
      in
      let splices = Runstatic.run_static ppf w_stat_lam in
      Translcore.set_splice_array (Some splices);
      let bytecode, required_globals =
        (typedtree, coercion)
        ++ Timings.(time (Transl sourcefile))
            (Translmod.transl_implementation modulename Nonstatic)
        ++ Timings.(accumulate_time (Generate sourcefile))
            (fun { Lambda.code = lambda; required_globals } ->
              print_if ppf Clflags.dump_rawlambda Printlambda.lambda lambda
              ++ Simplif.simplify_lambda sourcefile
              ++ print_if ppf Clflags.dump_lambda Printlambda.lambda
              ++ Bytegen.compile_implementation modulename
              ++ print_if ppf Clflags.dump_instr Printinstr.instrlist
              ++ fun bytecode -> bytecode, required_globals)
      in
      let objfile = outputprefix ^ ".cmo" in
      let oc = open_out_bin objfile in
      begin try
        bytecode
        ++ Timings.(accumulate_time (Generate sourcefile))
            (Emitcode.to_file oc modulename objfile ~required_globals);
        Warnings.check_fatal ();
        close_out oc;
        Stypes.dump (Some (outputprefix ^ ".annot"))
      with x ->
        close_out oc;
        remove_file objfile;
        raise x
      end;
      (* Save static code (if different from unit) *)
      if sstat_lam <> Lambda.lambda_unit then begin
        let stat_bytecode =
          Bytegen.compile_implementation modulename sstat_lam in
        let static_objfile = outputprefix ^ ".cmm" in
        let s_oc = open_out_bin static_objfile in
        try
          stat_bytecode
          ++ (Emitcode.to_file s_oc modulename static_objfile
               ~required_globals:Ident.Set.empty);
          close_out s_oc
        with x ->
          close_out s_oc;
          remove_file static_objfile;
          raise x
      end
    end
  with x ->
    Stypes.dump (Some (outputprefix ^ ".annot"));
    raise x
