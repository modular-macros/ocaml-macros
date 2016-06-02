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
open Asttypes

let rec module_let_kind m =
  match m.mod_desc with
  | Tmod_ident _ -> Alias
  | Tmod_constraint (m,_,_,_) -> module_let_kind m
  | _ -> Strict

let rec transl_implementation module_name str (* module coercion unhandled *) =
  let module_id = Ident.create_persistent module_name in
  let body = transl_structure [] str.str_items in
  Lprim (Psetglobal module_id, [body])

and transl_structure fields = function
  | [] ->
      Lprim(Pmakeblock (0, Immutable, None),
        List.map (fun id ->
          if Ident.name id = "0" then Lconst(Const_base(Const_int 0))
          else Lvar id)
          (List.rev fields))
  | item :: rem ->
    begin
      match item.str_desc with
      | Tstr_value(Asttypes.Static, rec_flag, pat_expr_list) ->
          let ext_fields = rev_let_bound_idents pat_expr_list @ fields in
          let body = transl_structure ext_fields rem in
          Translcore.transl_let rec_flag pat_expr_list body
      | Tstr_value(Asttypes.Nonstatic, _, pat_expr_list) ->
          let placeholders = List.map (fun _ -> Ident.create_persistent "0")
            @@ rev_let_bound_idents pat_expr_list
          in
          transl_structure (placeholders @ fields) rem
      | Tstr_module mb ->
          let id = mb.mb_id in
          let body = transl_structure (id :: fields) rem in
          let module_body =
            transl_module mb.mb_expr
          in
          Llet(module_let_kind mb.mb_expr, Pgenval, id,
               module_body,
               body)
      | _ -> (* other items ignored *)
          transl_structure fields rem
    end

and transl_module m =
  match m.mod_desc with
  | Tmod_structure str -> transl_structure [] str.str_items
    (* other module expressions not supported *)
  | _ -> lambda_unit
    (* might result in segfaults if these unsupported constructions are used
     * with static code in them *)

