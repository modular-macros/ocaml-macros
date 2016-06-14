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

(* Access paths *)

type t =
    Pident of Ident.t
  | Pdot of t * string * int
  | Papply of t * t

val same: t -> t -> bool
val compare: t -> t -> int
val isfree: Ident.t -> t -> bool
val binding_time: t -> int

val nopos: int

val name: ?paren:(string -> bool) -> t -> string
    (* [paren] tells whether a path suffix needs parentheses *)
val head: t -> Ident.t

val heads: t -> Ident.t list

val last: t -> string

type typath =
  | Regular of t
  | Ext of t * string
  | LocalExt of Ident.t
  | Cstr of t * string

val constructor_typath: t -> typath
val is_constructor_typath: t -> bool

(** Returns [true] iff the left-most identifier begins with a caret (`^`). *)
val is_lifted: t -> bool

(** Removes the first character of its argument if it is a caret (`^`). *)
val unlift: t -> t

(** Removes the lifting symbol in path (if any) iff the path begins with a
    persistent identifier (see [Ident]). WARNING: creates a fresh identifier if
    called on a non-global path. *)
val unlift_string: string -> string

(** Lifts a path if it is not already lifted. WARNING: creates a new fresh
   identifier if called on a non-global path. *)
val lift: t -> t

