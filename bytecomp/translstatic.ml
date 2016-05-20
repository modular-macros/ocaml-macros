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

open Typedtree
open Lambda
open Tast_mapper
open Asttypes

let clear_lifting sub expr =
  let desc =
    match expr.exp_desc with
    | Texp_ident (p, loc, desc) ->
        Texp_ident(Path.unlift p, loc, desc)
    | _ -> (default.expr sub expr).exp_desc
  in
  {expr with exp_desc = desc}

let clear_lifting_mapper =
  {Tast_mapper.default with expr =
    fun sub e -> clear_lifting sub e}

let rec module_let_kind m =
  match m.mod_desc with
  | Tmod_ident _ -> Alias
  | Tmod_constraint (m,_,_,_) -> module_let_kind m
  | _ -> Strict

let rec transl_implementation str (* module coercion unhandled *) =
  let str = clear_lifting_mapper.structure clear_lifting_mapper str in
  transl_structure [] str.str_items

and transl_structure fields = function
  | [] ->
      Lprim(Pmakeblock (0, Immutable, None),
        List.map (fun id -> Lvar id) (List.rev fields))
  | item :: rem ->
    begin
      match item.str_desc with
      | Tstr_value(Asttypes.Static, rec_flag, pat_expr_list) ->
          let ext_fields = rev_let_bound_idents pat_expr_list @ fields in
          let body = transl_structure ext_fields rem in
          Translcore.transl_let rec_flag pat_expr_list body
      | Tstr_module mb ->
          let id = mb.mb_id in
          let body = transl_structure (id :: fields) rem in
          let module_body =
            transl_module mb.mb_expr
          in
          Llet(module_let_kind mb.mb_expr, Pgenval, id,
               module_body,
               body)
      | _ -> (* everything but static bindings is ignored *)
          transl_structure fields rem
    end

and transl_module m =
  match m.mod_desc with
  | Tmod_structure str -> transl_structure [] str.str_items
  | _ (* rest not supported *) ->
      lambda_unit

