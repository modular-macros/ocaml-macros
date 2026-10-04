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

(* Reporting a failure of compile-time evaluation from a static
   program.  The failure happens in a process that the driver runs, out
   of reach of the driver's own reporting, so it is written to a file
   that the driver reads.

   The static program calls [run] by its position in this unit, 0, as
   it calls [Static_holes.write].  This interface must therefore export
   no other value. *)

type failure = Location.t * string
(** An error, to be reported at the location with the message *)

val run : string -> Location.t -> string -> (unit -> Obj.t) -> Obj.t
(** [run err_file loc what f] is [f ()].  If [f] raises, the failure is
    written to [err_file] as a marshalled [failure], and the process
    exits with a nonzero status.  The failure is an error at [loc] whose
    message is [Compile_time_failure.message what exn]. *)
