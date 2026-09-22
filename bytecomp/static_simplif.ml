(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let max_static_exception lam =
  let m = ref 0 in
  let rec scan lam =
    (match lam with
     | Lambda.Lstaticraise (i, _) | Lambda.Lstaticcatch (_, (i, _), _) ->
         if i > !m then m := i
     | _ -> ());
    Lambda.iter_head_constructor scan lam
  in
  scan lam;
  !m

let adopt_ident_stamps lam =
  let rec scan lam =
    (match lam with
     | Lambda.Lvar id | Lambda.Lmutvar id
     | Lambda.Llet (_, _, id, _, _) | Lambda.Lmutlet (_, id, _, _)
     | Lambda.Ltrywith (_, id, _) | Lambda.Lfor (id, _, _, _, _)
     | Lambda.Lassign (id, _) | Lambda.Lifused (id, _) ->
         Ident.reserve_stamp id
     | Lambda.Lletrec (binds, _) ->
         List.iter (fun b -> Ident.reserve_stamp b.Lambda.id) binds
     | Lambda.Lstaticcatch (_, (_, vars), _) ->
         List.iter (fun (v, _) -> Ident.reserve_stamp v) vars
     | Lambda.Lfunction lfun ->
         List.iter (fun (p, _) -> Ident.reserve_stamp p) lfun.Lambda.params
     | _ -> ());
    Lambda.iter_head_constructor scan lam
  in
  scan lam

let simplify lam =
  Lambda.reserve_raise_count (max_static_exception lam);
  adopt_ident_stamps lam;
  Simplif.simplify_lambda lam
