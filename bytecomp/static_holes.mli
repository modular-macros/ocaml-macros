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

(* Writing the terms that fill the holes of a unit's run-time term, from
   the static program of the unit, for the compiler to read.

   The static program calls [write] by its position in this unit, 0, as
   it calls [Static_report.run].  This interface must therefore export
   no other value. *)

val write : string -> Obj.t -> unit
(** [write file holes] writes [holes], the block of the terms, to
    [file]. *)
