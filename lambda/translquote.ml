open Misc
open Lambda
open Debuginfo

let stdmod_path = "CamlinternalQuote"

let camlinternalQuote =
  lazy
    (let mod_ident = Ident.create_persistent stdmod_path in
     let env' = Env.add_persistent_structure mod_ident Env.initial in
     match Env.open_pers_signature stdmod_path env' with
     | Error `Not_found -> fatal_errorf "Module %s unavailable." stdmod_path
     | Ok env -> env)

let combinator s =
  lazy
    (let lazy env = camlinternalQuote in
     let lid = Parse.val_ident (Lexing.from_string s) in
     match Env.find_value_by_name lid env with
     | path, _ -> Lambda.transl_value_path Loc_unknown env path
     | exception Not_found -> fatal_errorf "Primitive %s not found" s)

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

let const : 'a. 'a -> lambda = fun x ->
  try (Obj.magic (CamlinternalLam.const x) : lambda)
  with Invalid_argument msg -> fatal_error msg

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

let finish ~tag args fvs = seq (check fvs) (pair (mkblock ~tag args) fvs)

let in_binder_scope vars e k =
  List.fold_right gensym vars (new_scope vars e %>> k)

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
  | Lsplice e -> e

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
  match sw_failaction with
  | None -> build none fvsc
  | Some d ->
      term d %>> fun cd fvsd ->
      merge fvsc fvsd >>> fun fvs ->
      build (some cd) fvs

and term t = quote_lambda t

let non_pattern_binders (e : Typedtree.expression) =
  match e.exp_desc with
  | Texp_for (x, _, _, _, _, _) -> [x]
  | Texp_letop { param; _ } -> [param]
  | Texp_function (params, body) ->
      List.map (fun fp -> fp.Typedtree.fp_param) params
      @ (match body with
         | Tfunction_cases { param; _ } -> [param]
         | Tfunction_body _ -> [])
  | _ -> []

let binder_collector ?(constructor_extra = fun _ -> ())
    ?(ext_path_extra = fun _ -> ()) ?(module_path_extra = fun _ -> ())
    bound expr_extra =
  let relative_level = ref 0 in
  let expr iter (e : Typedtree.expression) =
    expr_extra e;
    List.iter (fun x -> bound := Ident.Set.add x !bound)
      (non_pattern_binders e);
    match e.exp_desc with
    | Typedtree.Texp_quote _ ->
        incr relative_level;
        Tast_iterator.default_iterator.expr iter e;
        decr relative_level
    | Typedtree.Texp_splice _ ->
        decr relative_level;
        Tast_iterator.default_iterator.expr iter e;
        incr relative_level
    | _ ->
        Tast_iterator.default_iterator.expr iter e
  in
  let quoted_constructor_extra cstr =
    if !relative_level >= 0 then constructor_extra cstr
  in
  let expr iter (e : Typedtree.expression) =
    (match e.Typedtree.exp_desc with
     | Typedtree.Texp_construct (_, cstr, _) ->
         quoted_constructor_extra cstr
     | Typedtree.Texp_extension_constructor (_, p) ->
         if !relative_level >= 0 then ext_path_extra p
     | _ -> ());
    expr iter e
  in
  let rebind_extra (ext : Typedtree.extension_constructor) =
    match ext.ext_kind with
    | Typedtree.Text_rebind (p, _) ->
        if !relative_level >= 0 then ext_path_extra p
    | Typedtree.Text_decl _ -> ()
  in
  let module_binder id = bound := Ident.Set.add id !bound in
  let structure_item iter item =
    (match item.Typedtree.str_desc with
     | Typedtree.Tstr_exception { tyexn_constructor = ext; _ } ->
         rebind_extra ext
     | Typedtree.Tstr_typext te ->
         List.iter rebind_extra te.Typedtree.tyext_constructors
     | Typedtree.Tstr_module mb ->
         Option.iter module_binder mb.Typedtree.mb_id
     | Typedtree.Tstr_recmodule mbs ->
         List.iter
           (fun mb -> Option.iter module_binder mb.Typedtree.mb_id) mbs
     | _ -> ());
    Tast_iterator.default_iterator.structure_item iter item
  in
  let module_expr iter (me : Typedtree.module_expr) =
    (match me.Typedtree.mod_desc with
     | Typedtree.Tmod_ident (p, _) ->
         if !relative_level >= 1 then module_path_extra p
     | Typedtree.Tmod_functor (_, Typedtree.Named (Some id, _, _), _) ->
         module_binder id
     | _ -> ());
    Tast_iterator.default_iterator.module_expr iter me
  in
  let pat : type k . _ -> k Typedtree.general_pattern -> unit =
    fun iter p ->
      (match p.Typedtree.pat_desc with
       | Typedtree.Tpat_construct (_, cstr, _, _) ->
           quoted_constructor_extra cstr
       | _ -> ());
      List.iter (fun i -> bound := Ident.Set.add i !bound)
        (Typedtree.pat_bound_idents p);
      Tast_iterator.default_iterator.pat iter p
  in
  { Tast_iterator.default_iterator with
    expr; pat; structure_item; module_expr }

let fv (env : Env.t) (t : Typedtree.expression) : Ident.Set.t =
  let occurrences = ref Ident.Set.empty
  and bound = ref Ident.Set.empty in
  let rec id = function
    | Env.Aident x -> x
    | Env.Adot (x,_) -> id x
  in
  let constructor_occurrence (cstr : Data_types.constructor_description) =
    match cstr.cstr_tag with
    | Data_types.Cstr_extension (p, _) ->
        let root = Path.head p in
        if not (Ident.global root || Ident.is_predef root) then begin
          match Env.find_constructor_address p env with
          | x -> occurrences := Ident.Set.add (id x) !occurrences
          | exception Not_found -> ()
        end
    | _ -> ()
  in
  let value_occurrence path =
    match Env.find_value_address path env with
    | x -> occurrences := Ident.Set.add (id x) !occurrences
    | exception Not_found ->
        begin match path with
        | Path.Pident x when not (Ident.global x) ->
            occurrences := Ident.Set.add x !occurrences
        | _ -> ()
        end
  in
  let occurrence : Typedtree.expression -> unit = function
    | { exp_desc = Texp_ident (_, _, { val_kind = Val_prim _ }) } ->
        ()
    | { exp_desc = Texp_ident (path, _, { val_staging_level = 0 }) } ->
          value_occurrence path
    | { exp_desc = Texp_letop { let_; ands; _ } } ->
        List.iter
          (fun (bop : Typedtree.binding_op) ->
             if bop.bop_op_val.Types.val_staging_level = 0 then
               value_occurrence bop.bop_op_path)
          (let_ :: ands)
    | _ -> ()
  in
  let ext_path_occurrence p =
    let root = Path.head p in
    if not (Ident.global root || Ident.is_predef root) then begin
      match Env.find_constructor_address p env with
      | x -> occurrences := Ident.Set.add (id x) !occurrences
      | exception Not_found -> ()
    end
  in
  let module_path_occurrence p =
    let root = Path.head p in
    if not (Ident.global root || Ident.is_predef root) then begin
      match Env.find_module_address p env with
      | x -> occurrences := Ident.Set.add (id x) !occurrences
      | exception Not_found ->
          begin match p with
          | Path.Pident x when not (Ident.global x) ->
              occurrences := Ident.Set.add x !occurrences
          | _ -> ()
          end
    end
  in
  let iter =
    binder_collector ~constructor_extra:constructor_occurrence
      ~ext_path_extra:ext_path_occurrence
      ~module_path_extra:module_path_occurrence bound occurrence
  in
  iter.expr iter t;
  Ident.Set.diff !occurrences !bound

let quote_expression = quote_lambda

let rec quote_access lam =
  let tag = Obj.(tag (repr lam)) in
  match lam with
  | Lvar id | Lmutvar id ->
      pair (mkblock ~tag [const id]) (empty ())
  | Lprim (p, es, loc) ->
      let cs = list (fun e -> fst (quote_access e)) es in
      pair (mkblock ~tag [const p; cs; const loc]) (empty ())
  | Lsplice e -> e
  | Lconst _ ->
      pair (const lam) (empty ())
  | _ ->
      Misc.fatal_error "Translquote.quote_access: not a run-time access"

let close_freshened params body_of =
  let params = Ident.Set.filter (fun r -> not (Ident.global r)) params in
  let subst =
    Ident.Set.fold (fun r m -> Ident.Map.add r (Ident.rename r) m)
      params Ident.Map.empty
  in
  let fresh r = Ident.Map.find r subst in
  let bind r body = Llet (Strict, Pgenval, fresh r, const r, body) in
  (Ident.Set.fold bind params) @@
    body_of (fun lam -> Lambda.rename subst lam)
      (List.map fresh (Ident.Set.elements params))

let transl_close_quotation _loc params body =
  close_freshened params @@ fun rename fresh_vars ->
  new_scope fresh_vars
    (rename body %>> fun c fvs -> seq (check fvs) (pair c fvs))

let transl_close_function _loc params body =
  let params = Ident.Set.filter (fun r -> not (Ident.global r)) params in
  let originals = Ident.Set.elements params in
  let subst =
    List.fold_left (fun m r -> Ident.Map.add r (Ident.rename r) m)
      Ident.Map.empty originals
  in
  let fresh_vars = List.map (fun r -> Ident.Map.find r subst) originals in
  let code =
    fst (new_scope fresh_vars
           (Lambda.rename subst body %>> fun c fvs ->
              seq (check fvs) (pair c fvs)))
  in
  (originals, fresh_vars, code)

let fv exp = fv exp.Typedtree.exp_env exp

let hole_args_as_constants = function
  | Lapply app ->
      let arg = function Lvar r -> const r | a -> a in
      Lapply { app with ap_args = List.map arg app.ap_args }
  | e -> e

let one q e = match q with None -> const e | Some c -> c

let rec quote lam =
  let tag = Obj.(tag (repr lam)) in
  let rebuild args = Some (mkblock ~tag args) in
  match lam with
  | Lsplice e -> Some (hole_args_as_constants e)
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
      fatal_error "Translquote.quote: Levent"
  | Lifused (id, e) ->
      begin match quote e with
      | None -> None
      | Some c -> rebuild [const id; c]
      end

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
             (match sw_failaction with
              | None -> const None
              | Some e -> some (one (Option.join qfail) e))])

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

let module_builder lam =
  let rec wrap_holes lam =
    match lam with
    | Lsplice e -> Lsplice (pair e (empty ()))
    | _ -> shallow_map wrap_holes lam
  in
  let lam =
    Lambda.subst (fun _ _ env -> env) ~freshen_bound_variables:true
      Ident.Map.empty lam
  in
  let lam = wrap_holes (remove_events lam) in
  quote_lambda lam %>> fun c fvs -> seq (check fvs) c
