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

open Misc
open Lambda
open Debuginfo

let combinator field = lazy (transl_prim "CamlinternalQuote" field)

let apply (lazy ap_func) ap_args =
  Lapply { ap_func; ap_args;
           ap_loc = Scoped_location.Loc_unknown;
           ap_tailcall = Default_tailcall;
           ap_inlined = Default_inline;
           ap_specialised = Default_specialise }

module Identifier = struct
  let rename = combinator "Identifier.rename"
  let empty = combinator "Identifier.empty"
  let free_var = combinator "Identifier.free_var"
  let new_scope = combinator "Identifier.new_scope"
  let merge_free_vars = combinator "Identifier.merge_free_vars"
  let check = combinator "Identifier.check"

end

module InternalVar = struct
  let counter = ref 0
  let fresh_name () =
    incr counter;
    Printf.ksprintf Ident.create_local "translquoteInternalVar_%d" !counter
end

(* A value that [CamlinternalLam.const] rejects is a bug in the compiler. *)
let const : 'a. 'a -> lambda = fun x ->
  try (Obj.magic (CamlinternalLam.const x) : lambda)
  with Invalid_argument msg -> fatal_error msg

(* [Pmakeblock] evaluates its arguments right to left, but the code that
   builds a quotation must run left to right, since the splices in it
   have effects: the arguments are bound in order. *)
let mkblock ?(tag=0) args =
  match args with
  | [] | [ _ ] -> Lprim (Pmakeblock (tag, Immutable, None), args, Loc_unknown)
  | _ ->
      let binds =
        List.map (fun a -> (InternalVar.fresh_name (), a)) args
      in
      List.fold_right
        (fun (x, a) body -> Llet (Strict, Pgenval, x, a, body))
        binds
        (Lprim (Pmakeblock (tag, Immutable, None),
                List.map (fun (x, _) -> Lvar x) binds, Loc_unknown))

let none = const None
let nil = const []
let some x = mkblock [x]
let cons hd tl = mkblock [hd; tl]
let list f l = List.fold_right (fun x -> cons (f x)) l nil
let pair x y = mkblock [x; y]
let fst ?(typ=Pointer) args =
  Lprim (Pfield (0, typ, Immutable), [args], Loc_unknown)
let snd ?(typ=Pointer) args =
  Lprim(Pfield (1, typ, Immutable), [args], Loc_unknown)

let merge fvs1 fvs2 = apply Identifier.merge_free_vars [fvs1; fvs2]

let seq e1 e2 = Lsequence (e1, e2)

let fresh_name = InternalVar.fresh_name

let bind e f =
  let x = fresh_name () in
  Llet (Strict, Pgenval, x, e, f (Lvar x))

let pair_bind e f =
  let x = fresh_name () in
  let a = fst (Lvar x) in
  let b = snd (Lvar x) in
  Llet(Strict, Pgenval, x, e, f a b)

let ( >>> ) = bind
let ( %>> ) = pair_bind

let gensym x body =
  Llet (Strict, Pgenval, x, apply Identifier.rename [const x], body)

let check x = apply Identifier.check [x]

let empty _ = Lazy.force Identifier.empty
let free_var id = apply Identifier.free_var [Lvar id]

let new_scope vars e =
  apply Identifier.new_scope [list (fun x -> Lvar x) vars;
                              Lambda.thunk (fresh_name ()) e]

(* The common tail of [quote_lambda]'s cases: check the rebuilt node's
   free-variable set for extrusion, then pair the node -- tag [tag],
   fields [args] -- with that set. *)
let finish ~tag args fvs = seq (check fvs) (pair (mkblock ~tag args) fvs)

(* The common binder shape: rename each of [vars] to a fresh identifier
   value (binding the original identifier to it), build [e] in a scope
   that registers the binders, and hand the resulting (code, free-set)
   pair to [k], in whose result the renamed identifiers are in scope. *)
let in_binder_scope vars e k =
  List.fold_right gensym vars (new_scope vars e %>> k)

(* Evaluate the builds of [items] left to right, once each: [f] gives an
   item's (code, free-set) pair, [g] wraps the item's code into a list
   element.  [k] receives the element list and the merged free set. *)
let rec bind_terms f g items k =
  match items with
  | [] -> k nil (empty ())
  | x :: rest ->
      f x %>> fun c fvs ->
      bind_terms f g rest @@ fun cs fvsr ->
      merge fvs fvsr >>> fun m ->
      k (cons (g x c) cs) m

let rec quote_lambda lam =
  let tag = Obj.(tag (repr lam)) in
  match lam with
  | Lvar id | Lmutvar id ->
      finish ~tag [Lvar id] (free_var id)
  | Lconst _ as cst -> pair (const cst) (empty ())
  | Lapply lapp ->
      quote_lambda_apply lapp %>> fun c fvs -> finish ~tag [c] fvs
  | Lfunction lfunction ->
      quote_lfunction lfunction %>> fun c fvs -> finish ~tag [c] fvs
  | Llet (lk, vk, id, e1, e2) ->
      term e1 %>> fun c1 fvs1 ->
        in_binder_scope [id] (term e2) @@ fun c2 fvs2 ->
          merge fvs1 fvs2 >>> fun fvs ->
            finish ~tag [const lk; const vk; Lvar id; c1; c2] fvs
  | Lmutlet (vk, id, e1, e2) ->
      term e1 %>> fun c1 fvs1 ->
        in_binder_scope [id] (term e2) @@ fun c2 fvs2 ->
          merge fvs1 fvs2 >>> fun fvs ->
            finish ~tag [const vk; Lvar id; c1; c2] fvs
  | Lletrec (binds, e) ->
      let params = List.map (fun {id; _} -> id) binds in
      List.fold_right gensym params @@ (
        bind_terms (fun {def; _} -> new_scope params (quote_lfunction def))
          (fun {id; _} c -> pair (Lvar id) c) binds @@ fun defs fvsd ->
        new_scope params (term e) %>> fun c fvs1 ->
        merge fvsd fvs1 >>> fun fvs ->
        finish ~tag [defs; c] fvs)
  | Lprim (p, es, loc) ->
      bind_terms term (fun _ c -> c) es @@ fun cs fvs ->
      finish ~tag [const p; cs; const loc] fvs
  | Lswitch (e, sw, loc) ->
      term e %>> fun c fvs1 ->
        quote_lambda_switch sw %>> fun sw fvs2 ->
        merge fvs1 fvs2 >>> fun fvs ->
          finish ~tag [c; sw; const loc] fvs
  | Lstringswitch (e, cases, eopt, loc) ->
      term e %>> fun c fvs1 ->
      bind_terms (fun (_, e) -> term e)
        (fun (str, _) c -> pair (const str) c) cases @@ fun cs fvsc ->
      merge fvs1 fvsc >>> fun fvs2 ->
      (* The default is an [option]: [None] stays a constant, and a
         present default contributes its code under [Some] and its free
         set to the merge -- it is not itself a (code, free-set) pair. *)
      (match eopt with
       | None -> finish ~tag [c; cs; none; const loc] fvs2
       | Some d ->
           term d %>> fun cd fvsd ->
           merge fvs2 fvsd >>> fun fvs ->
           finish ~tag [c; cs; some cd; const loc] fvs)
  | Lstaticraise (i, es) ->
      bind_terms term (fun _ c -> c) es @@ fun cs fvs ->
      finish ~tag [const i; cs] fvs
  | Lstaticcatch (e1, (i, ids), e2) ->
      (* The handler's variables are binders: rename them and open a scope
         around the handler's build, as [Ltrywith] does, and put the
         renamed identifiers -- not constants -- in the rebuilt payload. *)
      let vars = List.map (fun (id, _) -> id) ids in
      term e1 %>> fun c1 fvs1 ->
      in_binder_scope vars (term e2) @@ fun c2 fvs2 ->
      merge fvs1 fvs2 >>> fun fvs ->
      finish ~tag
        [c1;
         pair (const i)
           (list (fun (id, vk) -> pair (Lvar id) (const vk)) ids);
         c2]
        fvs
  | Ltrywith (e1, id, e2) ->
      term e1 %>> fun c1 fvs1 ->
        in_binder_scope [id] (term e2) @@ fun c2 fvs2 ->
          merge fvs1 fvs2 >>> fun fvs ->
            finish ~tag [c1; Lvar id; c2] fvs
  | Lifthenelse (e1, e2, e3) ->
      term e1 %>> fun c1 fvs1 ->
        term e2 %>> fun c2 fvs2 ->
          term e3 %>> fun c3 fvs3 ->
            merge (merge fvs1 fvs2) fvs3 >>> fun fvs ->
              finish ~tag [c1; c2; c3] fvs
  | Lsequence (e1, e2)
  | Lwhile (e1, e2) ->
      term e1 %>> fun c1 fvs1 ->
        term e2 %>> fun c2 fvs2 ->
          merge fvs1 fvs2 >>> fun fvs ->
            finish ~tag [c1; c2] fvs
  | Lfor (id, e1, e2, dir, e3) ->
      term e1 %>> fun c1 fvs1 ->
        term e2 %>> fun c2 fvs2 ->
          in_binder_scope [id] (term e3) @@ fun c3 fvs3 ->
            merge (merge fvs1 fvs2) fvs3 >>> fun fvs ->
              finish ~tag [Lvar id; c1; c2; const dir; c3] fvs
  | Lassign (id, e) ->
      (* The target is referenced as an [Lmutvar] is: by the renamed
         identifier of the enclosing [Lmutlet]. *)
      term e %>> fun c fvs1 ->
        merge (free_var id) fvs1 >>> fun fvs ->
          finish ~tag [Lvar id; c] fvs
  | Lsend (mk, e1, e2, es, loc) ->
      term e1 %>> fun c1 fvs1 ->
        term e2 %>> fun c2 fvs2 ->
          merge fvs1 fvs2 >>> fun fvs3 ->
            bind_terms term (fun _ c -> c) es @@ fun cs fvse ->
            merge fvs3 fvse >>> fun fvs ->
            finish ~tag [const mk; c1; c2; cs; const loc] fvs
  | Levent (e, _)
  | Lifused (_, e) -> term e %>> fun c fvs -> seq (check fvs) (pair c fvs)
  | Lsplice sp -> splice_contents sp

and quote_lfunction { kind; params; return; body; attr; loc } =
  let vars = List.map (fun (x, _) -> x) params in
  in_binder_scope vars (term body) @@ fun c fvs ->
    pair (mkblock [const kind;
                   list (fun (i, k) -> pair (Lvar i) (const k)) params;
                   const return;
                   c;
                   const attr;
                   const loc])
      fvs
and quote_lambda_apply { ap_func; ap_args; ap_loc; ap_tailcall;
                         ap_inlined; ap_specialised } =
  term ap_func %>> fun c1 fvs1 ->
  bind_terms term (fun _ c -> c) ap_args @@ fun cs fvsa ->
  merge fvs1 fvsa >>> fun fvs ->
  let args = [c1; cs;
              const ap_loc;
              const ap_tailcall;
              const ap_inlined;
              const ap_specialised] in
  pair (mkblock args) fvs

and quote_lambda_switch { sw_numconsts; sw_consts; sw_numblocks;
                          sw_blocks; sw_failaction } =
  let case (i, _) c = pair (const i) c in
  bind_terms (fun (_, e) -> term e) case sw_consts @@ fun cs_consts fvs1 ->
  bind_terms (fun (_, e) -> term e) case sw_blocks @@ fun cs_blocks fvs2 ->
  merge fvs1 fvs2 >>> fun fvsc ->
  let build fail fvs =
    pair (mkblock [const sw_numconsts;
                   cs_consts;
                   const sw_numblocks;
                   cs_blocks;
                   fail])
      fvs
  in
  (* As in [Lstringswitch]: the failaction is an [option] of code, not a
     (code, free-set) pair. *)
  match sw_failaction with
  | None -> build none fvsc
  | Some d ->
      term d %>> fun cd fvsd ->
      merge fvsc fvsd >>> fun fvs ->
      build (some cd) fvs

and term t = quote_lambda t

let quote_expression = quote_lambda
