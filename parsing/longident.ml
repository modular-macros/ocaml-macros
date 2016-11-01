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

type t =
    Lident of string
  | Ldot of t * string
  | Lapply of t * t
  | Lglobal of string
  | Lfrommacro of t * int

let field_to_string i = "(" ^ string_of_int i ^ ")"

let rec flat accu = function
    Lident s
  | Lglobal s -> s :: accu
  | Lfrommacro(lid, i) -> flat (field_to_string i :: accu) lid
  | Ldot(lid, s) -> flat (s :: accu) lid
  | Lapply(_, _) -> Misc.fatal_error "Longident.flat"

let flatten lid = flat [] lid

let last = function
    Lident s
  | Lglobal s -> s
  | Lfrommacro(_, i) -> field_to_string i
  | Ldot(_, s) -> s
  | Lapply(_, _) -> Misc.fatal_error "Longident.last"

let rec split_at_dots s pos =
  try
    let dot = String.index_from s pos '.' in
    String.sub s pos (dot - pos) :: split_at_dots s (dot + 1)
  with Not_found ->
    [String.sub s pos (String.length s - pos)]

let parse s =
  match split_at_dots s 0 with
    [] -> Lident ""  (* should not happen, but don't put assert false
                        so as not to crash the toplevel (see Genprintval) *)
  | hd :: tl -> List.fold_left (fun p s -> Ldot(p, s)) (Lident hd) tl

let rec is_lifted = function
  | Lident s
  | Lglobal s -> String.length s <> 0 && s.[0] == '^'
  | Lfrommacro (id, _)
  | Ldot (id, _) -> is_lifted id
  | Lapply (id, _) -> is_lifted id

let rec lift = function
  | Lident s as l ->
      if String.length s <> 0 && s.[0] == '^' then l else Lident ("^" ^ s)
  | Lglobal s as l ->
      if String.length s <> 0 && s.[0] == '^' then l else Lglobal ("^" ^ s)
  | Lfrommacro (id, s) -> Lfrommacro (lift id, s)
  | Ldot (id, s) -> Ldot (lift id, s)
  | Lapply (_,_) -> Misc.fatal_error "Longident.lift on functor"
