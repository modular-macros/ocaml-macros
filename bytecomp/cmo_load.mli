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
    .cmo files loading *)

(** Exception raised in case of failure in loading an object file. *)
exception Load_failed

(** [load_file recursive fmt name before_ld after_ld on_failure]
    @param recursive If [true], will load recursively all compilation units
      required by [name], otherwise won't.
    @param fmt The formatter for error messages.
    @param name The name of the file to load, without extension.
    @param before_ld Function that will be called before executing the contents
      of the object file.
    @param after_ld Same as [before_ld], but will be called after the execution.
    @param on_failure Function called in case the load failed.
 *)
val load_file : bool -> Format.formatter -> string
  -> (unit -> unit) -> (unit -> unit) -> (exn -> unit)
  -> bool

