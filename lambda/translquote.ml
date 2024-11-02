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
end

let const : 'a. 'a -> lambda =
  let rec constant_of : 'a. 'a -> structured_constant = fun o ->
    let o = Obj.repr o in
    if Obj.is_int o then Const_int (Obj.obj o)
    else if Obj.tag o = Obj.string_tag
    then Const_immstring (Obj.obj o)
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

let gensym x body =
  Llet(Strict, Pgenval, x, apply Identifier.rename [const x], body)
let gensym_no_rename x body =
  Llet(Strict, Pgenval, x, const x, body)

let rec quote_lambda lam =
  let tag = Obj.(tag (repr lam)) in
  match lam with
  | Lvar id -> mkblock ~tag [Lvar id]
  | Lmutvar id -> mkblock ~tag [Lvar id]
  | Lconst _ as cst -> const cst
  | Lapply lapp -> mkblock ~tag [quote_lambda_apply lapp]
  | Lfunction lfunction -> mkblock ~tag [quote_lfunction lfunction]
  | Llet (lk, vk, id, e1, e2) ->
     gensym id @@ mkblock ~tag [const lk; const vk; Lvar id; term e1; term e2]
  | Lmutlet (vk, id, e1, e2) ->
     gensym id @@ mkblock ~tag [const vk; Lvar id; term e1; term e2]
  | Lletrec (binds, e) ->
     List.fold_right (fun {id;_} -> gensym id) binds @@
       mkblock ~tag [list (fun {id; def} -> pair (Lvar id) (quote_lfunction def)) binds;
                     term e]
  | Lprim (p, es, loc) -> mkblock ~tag [const p; list term es; const loc]
  | Lswitch (e, sw, loc) -> mkblock ~tag [term e; quote_lambda_switch sw; const loc]
  | Lstringswitch (e, cases, eopt, loc) ->
     mkblock ~tag [term e;
                   list (fun (str, e') -> pair (const str) (term e')) cases;
                   option term eopt;
                   const loc]
  | Lstaticraise (i, es) -> mkblock ~tag [const i; list term es]
  | Lstaticcatch (e1, (i, ids), e2) -> mkblock ~tag [term e1; const (i, ids); term e2]
  | Ltrywith (e1, id, e2) -> gensym id @@ mkblock ~tag [term e1; Lvar id; term e2]
  | Lifthenelse (e1, e2, e3) -> mkblock ~tag [term e1; term e2; term e3]
  | Lsequence (e1, e2)
  | Lwhile (e1, e2) -> mkblock ~tag [term e1; term e2]
  | Lfor (id, e1, e2, dir, e3) ->
     gensym id @@ mkblock ~tag [Lvar id; term e1; term e2; const dir; term e3]
  | Lassign (id, e) -> mkblock ~tag [const id; term e]
  | Lsend (mk, e1, e2, es, loc) ->
     mkblock ~tag [const mk; term e1; term e2; list term es; const loc]
  | Levent (e, _)
  | Lifused (_, e) -> term e
  | Lsplice e -> e

and quote_lfunction { kind; params; return; body; attr; loc } =
  List.fold_right (fun (x,_) -> gensym x) params @@
    mkblock [const kind;
             list (fun (i, k) -> pair (Lvar i) (const k)) params;
             const return;
             term body;
             const attr;
             const loc]
and quote_lambda_apply { ap_func; ap_args; ap_loc; ap_tailcall;
                         ap_inlined; ap_specialised } =
  mkblock [term ap_func; list term ap_args; const ap_loc; const ap_tailcall;
           const ap_inlined; const ap_specialised]
and quote_lambda_switch { sw_numconsts; sw_consts; sw_numblocks;
                          sw_blocks; sw_failaction } =
  mkblock [const sw_numconsts;
           list (fun (i, e') -> pair (const i) (term e')) sw_consts;
           const sw_numblocks;
           list (fun (i, e') -> pair (const i) (term e')) sw_blocks;
           option term sw_failaction]
and term t = quote_lambda t

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

let transl_close_expression _loc =  Ident.Set.fold gensym_no_rename

let fv exp = fv exp.Typedtree.exp_env exp
