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

(** runstatic.ml: Execution of static terms and splices. *)
(** WARNING WARNING This interface is implemented by two different files,
 * byterunstatic.ml (for ocaml and ocamlopt) and optrunstatic.ml (for
 * ocamlc.opt and ocamlopt.opt). The command generating runstatic.ml is
 * executed at the Makefile level. DO NOT edit runstatic.ml directly!
 *)

open Lambda

(** Run the static code for an implementation and return the array of splices.
    *)
val run_static : Format.formatter -> lambda -> Parsetree.expression array

val load_static_deps : Format.formatter -> (Cmo_format.reloc_info * int) list
  -> unit

