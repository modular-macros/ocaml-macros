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

(** translstatic.ml: Translation of static declarations to lambda terms for the
    module language. *)

open Typedtree
open Lambda

(** Translates to lambda terms all static bindings (i.e. bindings declared with
    the [static] keyword), ignore other declarations. *)
(* module coercion not handled *)
val transl_implementation : string -> structure -> lambda

