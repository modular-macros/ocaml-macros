(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Long identifiers, used in parsetree. *)

type t =
    Lident of string
  | Ldot of t * string
  | Lapply of t * t
  | Lglobal of string
      (* [Lglobal] represents an identifier that can only be interpreted as
         global, whatever is in scope. It is used to avoid that global
         identifiers be shadowed by local ones after macro expansion.
         This variant should only be created by macros, and never by the
         parser. *)
  | Lfrommacro of t * int
      (* Used to refer to an element of a path closure after macro expansion.
         This variant should only be created by macros, and never by the
         parser. *)

val flatten: t -> string list
val last: t -> string
val parse: string -> t

(** Returns [true] iff the left-most identifier begins with a caret (`^`). *)
val is_lifted: t -> bool

(** [lift id] adds a lifting symbol (`^`) at the beginning of [id] iff it is not
    already present. *)
val lift: t -> t

