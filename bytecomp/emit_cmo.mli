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

(* Writing a bytecode object from a lambda term: the last step of the
   compilation of a unit, of its static program and of its compile-time
   part. *)

val to_cmo :
  source_file:string ->
  prefix:string ->
  required_globals:Ident.Set.t ->
  Lambda.lambda ->
  unit
(** [to_cmo ~source_file ~prefix ~required_globals lam] compiles [lam] and
    writes the object to [prefix ^ ".cmo"], removing a partly written file
    if anything raises. *)
