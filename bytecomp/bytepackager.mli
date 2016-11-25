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

(* "Package" a set of .cmo,.cmm pairs into one .cmo and one .cmm file having the
   original compilation units as sub-modules. The file names are expected
   to end in ".cmo", although both a ".cmo" and a ".cmm" will be produced. For
   each ".cmo", a corresponding ".cmm" is expected in the path. *)

val package_files: Format.formatter -> Env.t -> string list -> string -> unit

type error =
    Forward_reference of string * Ident.t
  | Multiple_definition of string * Ident.t
  | Not_an_object_file of string
  | Illegal_renaming of string * string * string
  | File_not_found of string

exception Error of error

val report_error: Format.formatter -> error -> unit
val reset: unit -> unit
