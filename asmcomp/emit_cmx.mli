(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

val to_cmx :
  source_file:string ->
  prefix:string ->
  module_name:string ->
  load_path:Load_path.paths ->
  flags:Clflags.emitter_flags ->
  primitives:Primitive.description list ->
  main_module_block_size:int ->
  required_globals:Ident.Set.t ->
  Lambda.lambda ->
  unit
