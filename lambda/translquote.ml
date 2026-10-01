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

(* Closing the code of a top-level splice over the run-time variables
   that it quotes.  The variables are renamed in each term that the
   splice quotes, but not in the rest of its code: [closing] maps each
   to the parameter that stands for it. *)
let rec rename_quoted subst lam =
  let renamed id = Option.value (Ident.Map.find_opt id subst) ~default:id in
  match lam with
  | Lvar id -> Lvar (renamed id)
  | Lmutvar id -> Lmutvar (renamed id)
  | Lassign (id, e) -> Lassign (renamed id, rename_quoted subst e)
  | Lsplice _ -> lam
  | _ -> Lambda.shallow_map (rename_quoted subst) lam

let close_quoted closing lam =
  if Ident.Map.is_empty closing then lam else rename_quoted closing lam

(* [transl_close_function loc params body] gives the variables of
   [params] that are not persistent, in a fixed order, the parameters
   that stand for them, and the code that [body] translates the splice
   to, in which each quoted term mentions the parameters.  The caller
   abstracts the code over the parameters and applies it, at the hole,
   to the variables, so that a renaming of the surrounding term reaches
   them.  A reference through a persistent identifier is quoted as a
   primitive and needs no closing. *)
let transl_close_function _loc params body =
  let params = Ident.Set.filter (fun r -> not (Ident.global r)) params in
  let originals = Ident.Set.elements params in
  let subst =
    List.fold_left (fun m r -> Ident.Map.add r (Ident.rename r) m)
      Ident.Map.empty originals
  in
  let fresh_vars = List.map (fun r -> Ident.Map.find r subst) originals in
  let body = body subst in
  let code =
    fst (new_scope fresh_vars
           (body %>> fun c fvs -> seq (check fvs) (pair c fvs)))
  in
  (originals, fresh_vars, code)

(* Rebuilding the lambda term of a unit.

   [quote lam] is code that builds [lam] with the contents of each hole
   in the place of the hole, or [None] if [lam] has no hole: the parent
   then embeds the whole of [lam] as one constant.  Unlike
   [quote_lambda], it keeps the binders of the term: the term is closed,
   so there is nothing to rename and no scope to check. *)

(* The contents of a hole are the application of a function of the
   static program to the variables of the term that the splice quotes.
   The term keeps its binders, so each variable is passed as the
   constant that denotes it. *)
let hole_contents { sp_code; sp_args } =
  match sp_args with
  | [] -> sp_code
  | _ ->
      Lapply { ap_func = sp_code;
               ap_args = List.map const sp_args;
               ap_loc = Loc_unknown;
               ap_tailcall = Default_tailcall;
               ap_inlined = Default_inline;
               ap_specialised = Default_specialise }

(* A child beside its optional rebuild: the rebuilt code where the child
   needed rebuilding, the child embedded as one constant where not. *)
let one q e = match q with None -> const e | Some c -> c

let rec quote lam =
  let tag = Obj.(tag (repr lam)) in
  (* [rebuild] is called only when some child needs rebuilding. *)
  let rebuild args = Some (mkblock ~tag args) in
  match lam with
  | Lsplice sp -> Some (hole_contents sp)
  | Lvar _ | Lmutvar _ | Lconst _ -> None
  | Lapply { ap_func; ap_args; ap_loc; ap_tailcall; ap_inlined;
             ap_specialised } ->
      let qf = quote ap_func and qargs = quote_list ap_args in
      if qf = None && qargs = None then None
      else
        rebuild [mkblock [one qf ap_func; one qargs ap_args;
                          const ap_loc; const ap_tailcall;
                          const ap_inlined; const ap_specialised]]
  | Lfunction lfun ->
      begin match quote_lfunction lfun with
      | None -> None
      | Some c -> rebuild [c]
      end
  | Llet (lk, vk, id, e1, e2) ->
      let q1 = quote e1 and q2 = quote e2 in
      if q1 = None && q2 = None then None
      else rebuild [const lk; const vk; const id; one q1 e1; one q2 e2]
  | Lmutlet (vk, id, e1, e2) ->
      let q1 = quote e1 and q2 = quote e2 in
      if q1 = None && q2 = None then None
      else rebuild [const vk; const id; one q1 e1; one q2 e2]
  | Lletrec (binds, e) ->
      let qbinds = List.map (fun b -> quote_lfunction b.def) binds in
      let qe = quote e in
      if qe = None && List.for_all (( = ) None) qbinds then None
      else
        let bind b q = mkblock [const b.id; one q b.def] in
        rebuild [list (fun x -> x) (List.map2 bind binds qbinds); one qe e]
  | Lprim (p, es, loc) ->
      begin match quote_list es with
      | None -> None
      | Some cs -> rebuild [const p; cs; const loc]
      end
  | Lswitch (e, sw, loc) ->
      let qe = quote e and qsw = quote_switch sw in
      if qe = None && qsw = None then None
      else
        rebuild [one qe e; one qsw sw; const loc]
  | Lstringswitch (e, cases, eopt, loc) ->
      let qcases = List.map (fun (_, e) -> quote e) cases in
      let qe = quote e and qopt = Option.map quote eopt in
      if qe = None && List.for_all (( = ) None) qcases
         && (qopt = None || qopt = Some None)
      then None
      else
        let case (str, e) q = mkblock [const str; one q e] in
        rebuild [one qe e;
                 list (fun x -> x) (List.map2 case cases qcases);
                 (* [qopt] is [Option.map quote eopt], so it is present
                    exactly when the default is: flattening picks out the
                    default's rebuild, if it needed one. *)
                 (match eopt with
                  | None -> const None
                  | Some e -> some (one (Option.join qopt) e));
                 const loc]
  | Lstaticraise (i, es) ->
      begin match quote_list es with
      | None -> None
      | Some cs -> rebuild [const i; cs]
      end
  | Lstaticcatch (e1, handler, e2) ->
      let q1 = quote e1 and q2 = quote e2 in
      if q1 = None && q2 = None then None
      else rebuild [one q1 e1; const handler; one q2 e2]
  | Ltrywith (e1, id, e2) ->
      let q1 = quote e1 and q2 = quote e2 in
      if q1 = None && q2 = None then None
      else rebuild [one q1 e1; const id; one q2 e2]
  | Lifthenelse (e1, e2, e3) ->
      let q1 = quote e1 and q2 = quote e2 and q3 = quote e3 in
      if q1 = None && q2 = None && q3 = None then None
      else rebuild [one q1 e1; one q2 e2; one q3 e3]
  | Lsequence (e1, e2) | Lwhile (e1, e2) ->
      let q1 = quote e1 and q2 = quote e2 in
      if q1 = None && q2 = None then None
      else rebuild [one q1 e1; one q2 e2]
  | Lfor (id, e1, e2, dir, e3) ->
      let q1 = quote e1 and q2 = quote e2 and q3 = quote e3 in
      if q1 = None && q2 = None && q3 = None then None
      else
        rebuild [const id; one q1 e1; one q2 e2; const dir; one q3 e3]
  | Lassign (id, e) ->
      begin match quote e with
      | None -> None
      | Some c -> rebuild [const id; c]
      end
  | Lsend (mk, e1, e2, es, loc) ->
      let q1 = quote e1 and q2 = quote e2 and qes = quote_list es in
      if q1 = None && q2 = None && qes = None then None
      else
        rebuild [const mk; one q1 e1; one q2 e2; one qes es; const loc]
  | Levent _ ->
      (* [quote_module_lambda] strips events before quoting: an event's
         environment cannot be serialised by [const]. *)
      fatal_error "Translquote.quote: Levent"
  | Lifused (id, e) ->
      begin match quote e with
      | None -> None
      | Some c -> rebuild [const id; c]
      end

(* [None] if no element needs rebuilding. *)
and quote_list es =
  let qs = List.map quote es in
  if List.for_all (( = ) None) qs then None
  else Some (list (fun x -> x) (List.map2 (fun e q -> one q e) es qs))

and quote_lfunction { kind; params; return; body; attr; loc } =
  match quote body with
  | None -> None
  | Some c ->
      let param (i, k) = mkblock [const i; const k] in
      Some (mkblock [const kind; list param params; const return; c;
                     const attr; const loc])

and quote_switch { sw_numconsts; sw_consts; sw_numblocks; sw_blocks;
                   sw_failaction } =
  let qconsts = List.map (fun (_, e) -> quote e) sw_consts in
  let qblocks = List.map (fun (_, e) -> quote e) sw_blocks in
  let qfail = Option.map quote sw_failaction in
  if List.for_all (( = ) None) qconsts && List.for_all (( = ) None) qblocks
     && (qfail = None || qfail = Some None)
  then None
  else
    let case (i, e) q = mkblock [const i; one q e] in
    Some (mkblock
            [const sw_numconsts;
             list (fun x -> x) (List.map2 case sw_consts qconsts);
             const sw_numblocks;
             list (fun x -> x) (List.map2 case sw_blocks qblocks);
             (* As in [Lstringswitch]: [qfail] is [Option.map quote
                sw_failaction], so its outer option follows the
                failaction's. *)
             (match sw_failaction with
              | None -> const None
              | Some e -> some (one (Option.join qfail) e))])

(* An event carries a typing environment, which [const] cannot embed.
   The contents of a hole are code to run, not to quote, and keep their
   events. *)
let rec remove_events lam =
  match lam with
  | Levent (lam, _) -> remove_events lam
  | Lsplice _ -> lam
  | _ -> shallow_map remove_events lam

let quote_module_lambda lam =
  let lam = remove_events lam in
  match quote lam with
  | None -> const lam
  | Some code -> code

(* The holes of a term, numbered in the order of [shallow_map]: the
   static program numbers the holes of the term it rebuilds, and the
   compiler those of its own term, events and all, alike. *)
let map_holes f lam =
  let n = ref 0 in
  let rec map lam =
    match lam with
    | Lsplice sp -> let i = !n in incr n; f i sp
    | _ -> shallow_map map lam
  in
  let lam = map lam in
  lam, !n

(* Code that rebuilds the term as [quote_module_lambda]'s does, so that
   the splices are evaluated in the same order, and gives the block of
   the contents of its holes, numbered as by [map_holes]. *)
let quote_module_holes lam =
  let holes = Ident.create_local "*holes*" in
  let lam, count =
    map_holes
      (fun i sp ->
         let c = Ident.create_local "*hole*" in
         Lsplice
           { sp_code =
               Llet (Strict, Pgenval, c, hole_contents sp,
                     Lsequence
                       (Lprim (Psetfield (i, Pointer, Assignment),
                               [Lvar holes; Lvar c], Loc_unknown),
                        Lvar c));
             sp_args = [] })
      (remove_events lam)
  in
  Llet (Strict, Pgenval, holes,
        Lprim (Pmakeblock (0, Mutable, None),
               List.init count (fun _ -> lambda_unit), Loc_unknown),
        Lsequence
          (Lprim (Pignore, [quote_module_lambda lam], Loc_unknown),
           Lvar holes))

(* The code of a hole keeps the numbers of the static exceptions that
   the compilation of its quotations drew, which a later pass of this
   compilation must not draw again: a pass that introduces a handler
   assumes that its number is fresh.  The binders of the code need no
   such care: a quotation renames them, with stamps that the compiler
   never draws. *)
let fill_holes lam (contents : lambda array) =
  let rec reserve lam =
    (match lam with
     | Lstaticraise (i, _) | Lstaticcatch (_, (i, _), _) ->
         Lambda.reserve_raise_count i
     | _ -> ());
    Lambda.iter_head_constructor reserve lam
  in
  Array.iter reserve contents;
  let lam, _ = map_holes (fun i _ -> contents.(i)) lam in
  lam
