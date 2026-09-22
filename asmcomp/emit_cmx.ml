(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Backend = struct
  let symbol_for_global' = Compilenv.symbol_for_global'
  let closure_symbol = Compilenv.closure_symbol
  let really_import_approx = Import_approx.really_import_approx
  let import_symbol = Import_approx.import_symbol
  let size_int = Arch.size_int
  let big_endian = Arch.big_endian
  let max_sensible_number_of_arguments =
    Proc.max_arguments_for_tailcalls - 1
end
let backend = (module Backend : Backend_intf.S)

let to_cmx ~source_file ~prefix ~module_name ~load_path ~flags ~primitives
    ~main_module_block_size ~required_globals lam =
  let unit_info =
    Unit_info.make ~check_modname:false ~source_file Impl prefix
  in
  Load_path.(init ~auto_include:no_auto_include
               ~visible:load_path.visible ~hidden:load_path.hidden);
  Clflags.set_emitter_flags flags;
  Translmod.primitive_declarations := primitives;
  Compilenv.reset ?packname:!Clflags.for_package module_name;
  let cmi =
    let prefix_cmi = Unit_info.cmi unit_info in
    if Sys.file_exists (Unit_info.Artifact.filename prefix_cmi)
    then prefix_cmi
    else
      Unit_info.Artifact.from_filename
        (Load_path.find_normalized (module_name ^ ".cmi"))
  in
  ignore (Env.read_signature cmi : Types.signature);
  let middle_end =
    if Config.flambda
    then Flambda_middle_end.lambda_to_clambda
    else Closure_middle_end.lambda_to_clambda
  in
  let program : Lambda.program =
    { Lambda.module_ident = Ident.create_persistent module_name;
      main_module_block_size;
      required_globals;
      code = lam }
  in
  Asmgen.compile_implementation
    ~backend
    ~prefixname:prefix
    ~middle_end
    ~ppf_dump:Format.err_formatter
    program;
  Compilenv.save_unit_info
    (Unit_info.Artifact.filename (Unit_info.cmx unit_info))
