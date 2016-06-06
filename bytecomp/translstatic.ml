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

(* ordered array of lambda blocks for stage-0 splices *)
let item_splices = ref ([] : lambda list)

module TranslSplicesIterator = struct
  open TypedtreeIter
  include DefaultIteratorArgument

  let depth = ref 0

  let enter_expression expr =
    match expr.exp_desc with
    | Texp_escape e ->
        if !depth = 0 then
          let body = Translcore.transl_exp e in
          item_splices := !item_splices @ [body];
        else
          ()
    | Texp_quote _ ->
        incr depth
    | _ -> ()

  let leave_expression expr =
    match expr.exp_desc with
    | Texp_quote _ ->
        decr depth
    | _ -> ()
end

module TranslSplices = TypedtreeIter.MakeIterator(TranslSplicesIterator)

let rec transl_structure fields = function
  | [] ->
      let module_body =
        Lprim (Pmakeblock (0, Immutable, None),
          List.map (fun id ->
            if Ident.name id = "0" then Lconst (Const_base (Const_int 0))
            else Lvar id)
            (List.rev fields))
      in
      module_body, []
  | item :: rem ->
      let str_lam, next_splices =
        begin
          match item.str_desc with
          | Tstr_value(Asttypes.Static, rec_flag, pat_expr_list) ->
              let ext_fields = rev_let_bound_idents pat_expr_list @ fields in
              let (body, splices) =
                transl_structure ext_fields rem in
              Translcore.transl_let rec_flag pat_expr_list body, splices
          | Tstr_value(Asttypes.Nonstatic, _, pat_expr_list) ->
              let placeholders = List.map (fun _ -> Ident.create_persistent "0")
                @@ rev_let_bound_idents pat_expr_list
              in
              transl_structure (placeholders @ fields) rem
          | Tstr_module mb ->
              let id = mb.mb_id in
              let (module_body, module_splices) =
                transl_module mb.mb_expr
              in
              let (body, body_splices) =
                transl_structure (id :: fields) rem
              in
              Llet(module_let_kind mb.mb_expr, Pgenval, id,
                   module_body,
                   body),
              body_splices @ module_splices
          | _ -> (* other items ignored *)
              transl_structure fields rem
        end
      in
      (* Find splices in current structure item *)
      TranslSplices.iter_structure_item item;
      let item_splices_with_ids =
        List.map (fun l -> (Ident.create "*splice*", l)) !item_splices
      in
      (* Add the code for running splices *)
      let wrap_let str_lam (splice_id, splice_lam) =
        Llet(Strict, Pgenval, splice_id, splice_lam, str_lam)
      in
      let lam = List.fold_left wrap_let str_lam item_splices_with_ids in
      item_splices := [];
      (lam, next_splices @ List.map fst item_splices_with_ids)

and transl_module m =
  match m.mod_desc with
  | Tmod_structure str -> transl_structure [] str.str_items
    (* other module expressions not supported *)
  | _ -> lambda_unit, []
    (* might result in segfaults if these unsupported constructions are used
     * with static code in them *)

(* Wrap a module construction lambda inside a lambda that declares the module
   global (as a side effect), constructs an array with all splices encountered
   in this module and returns it. [splice_ids] should refer to these splices.
   *)
let rec insert_splice_array module_id splice_ids = function
  | Llet (a,b,c,lam,rem) ->
      Llet (a, b, c, lam, insert_splice_array module_id splice_ids rem)
  | Lletrec (vbs, rem) ->
      Lletrec (vbs, insert_splice_array module_id splice_ids rem)
  | Lprim (Pmakeblock _, _) as block ->
      let splice_body =
        Lprim (Pmakearray (Paddrarray, Mutable),
          List.map
          (fun id ->
            Translquote.transl_close_expression Location.none (Lvar id))
          (List.rev splice_ids))
      in
      Lsequence (
        Lprim (Psetglobal module_id, [block]),
        splice_body)
  | _ -> assert false

let transl_implementation module_name str (* module coercion unhandled *) =
  let module_id = Ident.create_persistent module_name in
  Translcore.transl_splices := false;
  let (mod_body, splice_ids) = transl_structure [] str.str_items in
  insert_splice_array module_id splice_ids mod_body

