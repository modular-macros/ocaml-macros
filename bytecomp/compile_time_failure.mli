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

(* The message of a failure of compile-time evaluation, in a static
   program or in the toplevel. *)

val message : string -> exn -> string
(** [message what exn] is the message of the exception [exn], raised by
    the compile-time evaluation of [what]: a splice, or a compile-time
    part.  The code built by [what] may have a variable escaping the
    scope of its quotation: the variables are named, and a variable that
    holds the evidence of an equation is reported as a use of the
    equation outside the match case where it holds.  Any other exception
    is given as [Printexc.to_string] gives it. *)
