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

module Identifier : sig
  type t

  type set_of_t

  (* Performed when a quotation mentions a variable bound outside every
     enclosing quotation scope. *)
  type _ Effect.t += FreeVar : set_of_t -> unit Effect.t

  val rename : t -> t

  val new_scope : t list -> (unit -> 'a * set_of_t) -> 'a * set_of_t

  val empty : set_of_t
  val free_var : t -> set_of_t
  val merge_free_vars: set_of_t -> set_of_t -> set_of_t
  val check : set_of_t -> unit

  (* The names of the variables of a set, sorted and without
     duplicates, for reporting a scope extrusion. *)
  val names : set_of_t -> string list
end
