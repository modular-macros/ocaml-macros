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
  (* let scope_extrusion_check = combinator "Identifier.scope_extrusion_check" *)

end

module InternalVar = struct 
  let counter = ref 0
  let fresh_name _ = incr counter; Ident.create_local ("translquoteInternalVar_" ^ (string_of_int !counter))
end

let const : 'a. 'a -> lambda =
  let rec constant_of : 'a. 'a -> structured_constant = fun o ->
    let o = Obj.repr o in
    if Obj.is_int o then Const_base (Const_int (Obj.obj o))
    else if Obj.tag o = Obj.string_tag
    then Const_base (Const_string (Obj.obj o, Location.none, None))
    else if Obj.is_block o && Obj.tag o <= Obj.last_non_constant_constructor_tag
    then Const_block (Obj.tag o,
                      List.init (Obj.size o) (fun i -> constant_of (Obj.field o i)))
    else Printf.ksprintf failwith "unsupported tag %d" (Obj.tag o)
  in
  fun x -> Lconst (constant_of x)

let mkblock ?(tag=0) args =
  Lprim (Pmakeblock(tag, Immutable, None), args, Loc_unknown)

let none = const None
let nil = const []
let some x = mkblock [x]
let cons hd tl = mkblock [hd; tl]
let option f = Option.fold ~some:(fun x -> some (f x)) ~none
let list f l = List.fold_right (fun x -> cons (f x)) l nil
let pair x y = mkblock [x; y]
let fst ?(typ=Pointer) args = Lprim(Pfield(0, typ, Immutable), [args], Loc_unknown)
let snd ?(typ=Pointer) args = Lprim(Pfield(1, typ, Immutable), [args], Loc_unknown)

let merge fvs1 fvs2 = apply Identifier.merge_free_vars [fvs1; fvs2]
let merge_fvs (l: Lambda.lambda list) (base: Lambda.lambda) = List.fold_left (fun acc x -> merge acc (snd x)) base l
(*generate fresh names*) 
(*make bind do let insertion*)

let seq e1 e2 = Lsequence(e1, e2)

let fresh_name _ = InternalVar.fresh_name()

let bind e f = (let x = fresh_name() in Llet(Strict, Pgenval, x, e, f (Lvar x)))
let pair_bind e f = (let x = fresh_name() in 
                    let a = fst (Lvar(x)) in 
                    let b = snd (Lvar(x)) in

                    Llet(Strict, Pgenval, x, e, f a b))

let ( >>> ) = bind
let ( %>> ) = pair_bind

let gensym x body =
  Llet(Strict, Pgenval, x, apply Identifier.rename [const x], body)

let gensym_no_rename x body =
  Llet(Strict, Pgenval, x, const x, body)

let check s x = apply Identifier.check [x; const s]

let empty _ = apply Identifier.empty [const ()]
let free_var id = (apply Identifier.free_var [Lvar id])

(*Creates a lambda function of type unit -> 'a*)

(*params: (Ident.t * value_kind) list;
    return: value_kind;
    body: lambda;
    attr: function_attribute; (* specified with [@inline] attribute *)
    loc: scoped_location; }*)

let new_scope name vars e = apply Identifier.new_scope [list (fun x -> Lvar x) vars; Lambda.thunk (fresh_name()) e; const name ]

(* let scope_extrusion_check e = apply Identifier.scope_extrusion_check [Lambda.thunk (fresh_name()) e] *)

(* let splice e = match e with 
  | Lprim (p, es, loc) -> (match p with 
     | Psetfield_computed _ 
     | Psetfield _ -> Lprim (p, List.map scope_extrusion_check es, loc)
     | _ -> e)
  | Lassign (id, e) -> Lassign(id, scope_extrusion_check e)
  | _ -> e *)

let rec quote_lambda_1 lam =
  let tag = Obj.(tag (repr lam)) in
  match lam with
  | Lvar id -> let fv = (free_var id) in seq (check "LMUTVAR" fv) (pair (mkblock ~tag [Lvar id]) (free_var id))
  | Lmutvar id -> let fv = (free_var id) in seq (check "LMUTVAR" fv) (pair (mkblock ~tag [Lvar id]) fv)
  | Lconst _ as cst -> pair (const cst) (empty ())
  | Lapply lapp -> quote_lambda_apply lapp %>> fun c fvs -> seq (check "LAPP" fvs) (pair (mkblock ~tag [c]) fvs) 
  | Lfunction lfunction -> quote_lfunction lfunction %>> fun c fvs -> seq (check "LFUN" fvs) (pair (mkblock ~tag [c]) fvs)
  | Llet (lk, vk, id, e1, e2) ->
     term e1 %>> fun c1 fvs1 -> 
      gensym id @@ (
        new_scope "LLET" [id] (term e2) %>> fun c2 fvs2 ->
          merge fvs1 fvs2 >>> fun fvs ->
            seq (check "LLET" fvs) 
                (pair (mkblock ~tag [const lk; const vk; Lvar id; c1; c2]) fvs))
  | Lmutlet (vk, id, e1, e2) ->
    term e1 %>> fun c1 fvs1 -> 
      gensym id @@ (
        new_scope "LMUTLET" [id] (term e2) %>> fun c2 fvs2 ->
          merge fvs1 fvs2 >>> fun fvs ->
            seq (check "LMUTLET" fvs) 
                (pair (mkblock ~tag [const vk; Lvar id; c1; c2]) fvs))
  | Lletrec (binds, e) ->
     let params = List.map (fun {id; _} -> id) binds in 
     let ts = List.map (fun {def;_} -> new_scope "LLETREC" params (quote_lfunction def)) binds in 
     let defs = List.combine params (List.map fst ts) in 
     List.fold_right gensym params @@
       new_scope "LLETREC" params (term e) %>> fun c fvs1 ->
        merge_fvs ts fvs1 >>> fun fvs ->
          seq (check "LLETREC" fvs) (pair (mkblock ~tag [list (fun (id, def) -> pair (Lvar id) (def)) defs; c]) fvs)
  | Lprim (p, es, loc) -> let ts = List.map term es in 
                          pair (list fst ts) (merge_fvs ts (empty ())) %>> fun cs fvs -> 
                            seq (check "LPRIM" fvs) (pair (mkblock ~tag [const p; cs; const loc]) fvs)
  | Lswitch (e, sw, loc) -> 
    term e %>> fun c fvs1 ->
      quote_lambda_switch sw %>> fun sw fvs2 -> 
        merge fvs1 fvs2 >>> fun fvs ->
          seq (check "LSWITCH" fvs)
              (pair (mkblock ~tag [c; sw; const loc])
                     fvs)
  | Lstringswitch (e, cases, eopt, loc) ->
     let strs, es = List.split cases in 
     let ts = List.map term es in 
     let cs = list (fun (str, t) -> pair (const str) (fst t)) (List.combine strs ts) in
     term e %>> fun c fvs1 ->
       pair (cs) (merge_fvs ts fvs1) %>> fun cs fvs2 ->
        option term eopt %>> fun copt fvs3 ->
          merge fvs2 fvs3 >>> fun fvs ->
            seq (check "LSTRINGSWITCH" fvs)
                ( pair (mkblock ~tag [c; cs; copt; const loc]) fvs)
  | Lstaticraise (i, es) -> let ts = List.map term es in 
                            pair (list fst ts) (merge_fvs ts (empty ())) %>> fun cs fvs -> 
                              seq (check "LSTATICRAISE"  fvs) (pair (mkblock ~tag [const i; cs]) fvs)
  | Lstaticcatch (e1, (i, ids), e2) -> term e1 %>> fun c1 fvs1 -> 
                                         term e2 %>> fun c2 fvs2 -> 
                                          merge fvs1 fvs2 >>> fun fvs ->
                                            seq (check "LSTATICCATCH" fvs)
                                                (pair (mkblock ~tag [c1; const (i, ids); c2]) fvs)
  | Ltrywith (e1, id, e2) -> term e1 %>> fun c1 fvs1 ->
                               gensym id @@ 
                               new_scope "LTRYWITH" [id] (term e2) %>> fun c2 fvs2 ->
                                merge fvs1 fvs2 >>> fun fvs ->
                                  seq (check "LTRYWITH" fvs) (pair (mkblock ~tag [c1; Lvar id; c2]) fvs)
  | Lifthenelse (e1, e2, e3) -> term e1 %>> fun c1 fvs1 ->
                                  term e2 %>> fun c2 fvs2 ->
                                    term e3 %>> fun c3 fvs3 ->
                                      merge (merge fvs1 fvs2) fvs3 >>> fun fvs ->
                                        seq (check "LIFTHENELSE" fvs)
                                            (pair (mkblock ~tag [c1; c2; c3]) fvs)
  | Lsequence (e1, e2)
  | Lwhile (e1, e2) -> term e1 %>> fun c1 fvs1 -> 
                         term e2 %>> fun c2 fvs2 -> 
                           merge fvs1 fvs2 >>> fun fvs ->
                             seq (check "L2" fvs) (pair (mkblock ~tag [c1; c2]) fvs)
  | Lfor (id, e1, e2, dir, e3) -> term e1 %>> fun c1 fvs1 ->
                                    term e2 %>> fun c2 fvs2 ->
                                      gensym id @@ 
                                      (new_scope "LFOR" [id] (term e3) %>> fun c3 fvs3 ->
                                        merge (merge fvs1 fvs2) fvs3 >>> fun fvs ->
                                          seq (check "LFOR" fvs) (pair (mkblock ~tag [Lvar id; c1; c2; const dir; c3]) fvs)
                                      )
  | Lassign (id, e) -> term e %>> fun c fvs -> seq (check "LASSIGN" fvs) (pair (mkblock ~tag [const id; c]) fvs)
  | Lsend (mk, e1, e2, es, loc) ->
     let ts = List.map term es in 
     term e1 %>> fun c1 fvs1 ->
      term e2 %>> fun c2 fvs2 ->
        merge fvs1 fvs2 >>> fun fvs3 ->
          pair (list fst ts) (merge_fvs ts fvs3) %>> fun cs fvs ->
            seq (check "LSEND" fvs) (pair (mkblock ~tag [const mk; c1; c2; cs; const loc]) fvs)
  | Levent (e, _)
  | Lifused (_, e) -> term e %>> fun c fvs -> seq (check "L1" fvs) (pair c fvs)
  | Lsplice e -> e

and quote_lfunction { kind; params; return; body; attr; loc } =
  let vars = List.map (fun (x, _) -> x) params in 
  List.fold_right (gensym) vars @@
    new_scope "LFUN" vars (term body) %>> fun c fvs -> 
      (pair (mkblock [const kind;
                     list (fun (i, k) -> pair (Lvar i) (const k)) params;
                     const return;
                     c;
                     const attr;
                     const loc])
            fvs)
and quote_lambda_apply { ap_func; ap_args; ap_loc; ap_tailcall;
                         ap_inlined; ap_specialised } =
  (term ap_func) 
    %>> fun c1 fvs1 -> 
        let ts = List.map term ap_args in 
        (pair (list fst ts) (merge_fvs ts fvs1))
          %>> fun cs fvs -> 
                    (pair (mkblock [c1; cs; const ap_loc; const ap_tailcall; const ap_inlined; const ap_specialised])  
                           fvs )      
                    
and quote_lambda_switch { sw_numconsts; sw_consts; sw_numblocks;
                          sw_blocks; sw_failaction } =
  let is, es1 = List.split sw_consts in 
  let js, es2 = List.split sw_blocks in 
  let ts1 = List.map term es1 in 
  let ts2 = List.map term es2 in 
  let consts = List.combine is ts1 in 
  let blocks = List.combine js ts2 in 
  pair (list (fun (i, t) -> pair (const i) (fst t)) consts) (merge_fvs ts1 (empty ())) %>> fun cs_consts fvs1 ->
     pair (list (fun (i, t) -> pair (const i) (fst t)) blocks) (merge_fvs ts2 fvs1) %>> fun cs_blocks fvs2 ->
      option term sw_failaction %>> fun c_failaction fvs3 -> 
        merge fvs2 fvs3 >>> fun fvs ->
              (pair (mkblock [const sw_numconsts;
                              cs_consts;
                              const sw_numblocks;
                              cs_blocks;
                              c_failaction])
                     fvs)

and quote_lambda lam = (quote_lambda_1 lam)
and term t = (quote_lambda_1 t)

(* TODO: this is an overapproximation: it actually just collects
   all the stage-0 variables, free or bound.  We can (and will, eventually,)
   do better, but this works for our use case for the moment *)
let fv (env : Env.t) (t : Typedtree.expression) : Ident.Set.t =
  let fvs = ref Ident.Set.empty in
  let rec id = function
    | Env.Aident x -> x
    | Env.Adot (x,_) -> id x
  in
  let expr iter = function
    | { Typedtree.exp_desc = Typedtree.Texp_ident (path, _lid, { val_staging_level = 0 }) } ->
        begin match Env.find_value_address path env with
        | x -> fvs := Ident.Set.add (id x) !fvs
        | exception Not_found -> ()
        end
    | e -> Tast_iterator.default_iterator.expr iter e
  in
  expr { Tast_iterator.default_iterator with expr } t;
  !fvs

let quote_expression =  quote_lambda

let transl_close_expression _loc params body =  (Ident.Set.fold gensym_no_rename params) @@ fst (new_scope "BASE" (Ident.Set.to_list params) 
                                                (body %>> fun c fvs -> seq (check "BASE" fvs) (pair c fvs)))

let fv exp = fv exp.Typedtree.exp_env exp
