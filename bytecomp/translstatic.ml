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

(* ordered array of lambda blocks for stage-0 splices *)
let item_splices = ref ([] : lambda list)

let transl_toplevel_splice exp =
  List.fold_left
    (fun lam id -> Translquote.wrap_local exp.exp_loc id.txt
      (Location.mkloc (Ident.name id.txt) id.loc) lam)
    (Translcore.transl_exp exp)
    (Env.cross_stage_ids exp.exp_env)

module TranslSplicesIterator = struct
  open TypedtreeIter
  include DefaultIteratorArgument

  let depth = ref 0

  let enter_expression expr =
    match expr.exp_desc with
    | Texp_escape e ->
        if !depth = 0 then
          let body = transl_toplevel_splice e in
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

let splice_ids = ref ([] : Ident.t list)

let transl_item_splices item item_lam =
  (* Find splices in current structure item *)
  TranslSplices.iter_structure_item item;
  let item_splices_with_ids =
    List.map (fun l -> (Ident.create "*splice*", l)) !item_splices
  in
  (* Add the code for running splices *)
  let wrap_let str_lam (splice_id, splice_lam) =
    Llet(Strict, Pgenval, splice_id, splice_lam, str_lam)
  in
  let lam = List.fold_left wrap_let item_lam item_splices_with_ids in
  item_splices := [];
  splice_ids := !splice_ids @ List.map fst item_splices_with_ids;
  lam

(* Wrap a module construction lambda inside a lambda that declares the module
   global (as a side effect), constructs an array with all splices encountered
   in this module and returns it. [splice_ids] should refer to these splices.
   *)
let rec insert_splice_array module_id splice_ids = function
  | Llet (a,b,c,lam,rem) ->
      Llet (a, b, c, lam, insert_splice_array module_id splice_ids rem)
  | Lletrec (vbs, rem) ->
      Lletrec (vbs, insert_splice_array module_id splice_ids rem)
  | block ->
      let splice_body =
        Lprim (Pmakearray (Paddrarray, Mutable),
          List.map
          (fun id ->
            Translquote.transl_close_expression Location.none (Lvar id))
          splice_ids)
      in
      Lsequence (
        Lprim (Psetglobal module_id, [block]),
        splice_body)

let transl_implementation module_name str cc =
  let module_id = Ident.create_persistent module_name in
  Translcore.set_transl_splices None;
  splice_ids := [];
  let (mod_body, _size) =
    Translmod.transl_structure [] cc (Some (Path.Pident module_id))
    Static transl_item_splices str.str_final_env str.str_items in
  insert_splice_array module_id !splice_ids mod_body

let toploop_ident = Ident.create_persistent "Toploop"
(*let toploop_getvalue_pos = 0 (* position of getvalue in module Toploop *)*)
let toploop_setvalue_pos = 1 (* position of setvalue in module Toploop *)

let toplevel_name id =
  Ident.name id

let toploop_setvalue id lam =
  Lapply{ap_should_be_tailcall=false;
         ap_loc=Location.none;
         ap_func=Lprim(Pfield toploop_setvalue_pos,
                       [Lprim(Pgetglobal toploop_ident, [])]);
         ap_args=[Lconst(Const_base(Const_string (toplevel_name id, None)));
                  lam];
         ap_inlined=Default_inline;
         ap_specialised=Default_specialise}

  (*
let toploop_getvalue id =
  Lapply{ap_should_be_tailcall=false;
         ap_loc=Location.none;
         ap_func=Lprim(Pfield toploop_getvalue_pos,
                       [Lprim(Pgetglobal toploop_ident, [])]);
         ap_args=[Lconst(Const_base(Const_string (toplevel_name id, None)))];
         ap_inlined=Default_inline;
         ap_specialised=Default_specialise}
  *)

let toploop_setvalue_id id = toploop_setvalue id (Lvar id)

let transl_toplevel_item item =
  let item_lam =
    match item.str_desc with
    | Tstr_eval (_, _) -> lambda_unit
    | Tstr_value (Static, rec_flag, pat_expr_list) ->
        let idents = let_bound_idents pat_expr_list in
        Translcore.transl_let rec_flag pat_expr_list
          (make_sequence toploop_setvalue_id idents)
    | _ -> lambda_unit
  in
  TranslSplices.iter_structure_item item;
  let item_splices_with_ids =
    List.map (fun l -> (Ident.create "*splice*", l)) !item_splices
  in
  let wrap_let str_lam (splice_id, splice_lam) =
    Llet (Strict, Pgenval, splice_id, splice_lam, str_lam)
  in
  let lam = List.fold_left wrap_let item_lam item_splices_with_ids in
  item_splices := [];
  splice_ids := !splice_ids @ List.map fst item_splices_with_ids;
  lam

let transl_toplevel_item_and_close itm =
  let itm_lam = transl_toplevel_item itm in
  Translmod.close_toplevel_term (itm_lam, ())

let rec insert_splice_array_toplevel splice_ids = function
  | Llet (a,b,c,lam,rem) ->
      Llet (a,b,c,lam, insert_splice_array_toplevel splice_ids rem)
  | Lletrec (vbs, rem) ->
      Lletrec (vbs, insert_splice_array_toplevel splice_ids rem)
  | other ->
      let splice_body =
        Lprim (Pmakearray (Paddrarray, Mutable),
          List.map
          (fun id ->
            Translquote.transl_close_expression Location.none
            (Lvar id))
          (List.rev splice_ids))
      in
      Lsequence (
        other,
        splice_body)

let transl_toplevel_definition str =
  match str.str_items with
  | [] -> lambda_unit
  | item :: _ -> (* Only first item handled! *)
    Translcore.set_transl_splices None;
    splice_ids := [];
    let def_body =
      transl_toplevel_item_and_close item
    in
    insert_splice_array_toplevel !splice_ids def_body

