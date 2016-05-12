(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Olivier Nicole, Paris-Saclay University                  *)
(*                                                                        *)
(*                      Copyright 2016 OCaml Labs                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** [load_cmo.ml]
    .cmo files loading into the symtable (used to run bytecode in the toplevel
    and when running static code and macros. *)

(** Exception raised in case of failure in loading an object file. *)
exception Load_failed

(** [load_file recursive fmt path name before_ld after_ld on_failure] loads a
    compilation unit (and its dependencies if [recursive] is [true]) into the
    bytecode symtable.
    @param recursive If [true], will load recursively all compilation units
      required by [name], otherwise won't.
    @param fmt The formatter for error messages.
    @param path The names of the directories in which to search for the file.
    @param name The name of the file to load, without extension.
    @param before_ld Function that will be called before executing the contents
      of the object file.
    @param after_ld Same as [before_ld], but will be called after the execution.
    @param on_failure Function called in case the load failed.
 *)
val load_file : bool -> Format.formatter -> string list -> string
  -> (unit -> unit) -> (unit -> unit) -> (exn -> unit)
  -> bool

