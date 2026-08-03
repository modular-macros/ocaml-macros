(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

val to_cmo :
  source_file:string ->
  prefix:string ->
  imports:Misc.crcs ->
  flags:Clflags.emitter_flags ->
  primitives:Primitive.description list ->
  required_globals:Ident.Set.t ->
  Lambda.lambda ->
  unit
