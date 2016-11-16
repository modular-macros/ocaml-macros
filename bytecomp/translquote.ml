open Misc
open Asttypes
open Types
open Typedtree
open Lambda

let stdmod_path = "^CamlinternalQuote"

let camlinternalQuote =
  lazy
    (match Env.open_pers_signature
       stdmod_path Env.initial_safe_string with
     | exception Not_found ->
         fatal_error @@ "Module " ^ stdmod_path ^ " unavailable."
     | env -> env)

let combinator modname field =
  lazy
    (let env = Lazy.force camlinternalQuote in
     let lid = Longident.Ldot(Longident.Lident modname, field) in
     match Env.lookup_value lid env with
     | (Path.Pdot(Path.Pdot(Path.Pident ident, _, pos1), _, pos2), _) ->
         Lprim(Pfield pos2,
               [Lprim(Pfield pos1,
                     [Lprim(Pgetglobal ident, [], Location.none)],
                      Location.none)],
               Location.none)
     | _ ->
         fatal_error @@
           "Primitive " ^ stdmod_path ^ "." ^ modname ^ "." ^ field
           ^ " not found."
     | exception Not_found ->
        fatal_error @@
          "Primitive " ^ stdmod_path ^ "." ^ modname ^ "." ^ field
          ^" not found.")

module Loc = struct
  let none = combinator "Loc" "none"
  let unmarshal = combinator "Loc" "unmarshal"
end

module Name = struct
  (*
  let mk = combinator "Name" "mk"
  *)
  let unmarshal = combinator "Name" "unmarshal"
end

(*
module Var = struct
  (*
  let name = combinator "Var" "name"
  *)
end
*)

module Constant = struct
  let unmarshal = combinator "Constant" "unmarshal"
end

module Identifier = struct
  let unmarshal = combinator "Ident" "unmarshal"
  let lfrommacro = combinator "Ident" "lfrommacro"
end

(*
module Label = struct
  let none = combinator "Label" "none"
  let of_string = combinator "Label" "of_string"
end
*)

module ArgLabel = struct
  let unmarshal = combinator "ArgLabel" "unmarshal"
end

module Variant = struct
  let of_string = combinator "Variant" "of_string"
end

module Method = struct
  let of_string = combinator "Method" "of_string"
end

module Pat = struct
  let any = combinator "Pat" "any"
  let var = combinator "Pat" "var"
  let alias = combinator "Pat" "alias"
  let constant = combinator "Pat" "constant"
  (*
  let interval = combinator "Pat" "interval"
  *)
  let tuple = combinator "Pat" "tuple"
  let construct = combinator "Pat" "construct"
  let variant = combinator "Pat" "variant"
  let record = combinator "Pat" "record"
  let array = combinator "Pat" "array"
  let or_ = combinator "Pat" "or_"
  (*
  let type_ = combinator "Pat" "type_"
  *)
  let lazy_ = combinator "Pat" "lazy_"
  let exception_ = combinator "Pat" "exception_"
end

module Case = struct
  let nonbinding = combinator "Case" "nonbinding"
  let simple = combinator "Case" "simple"
  let pattern = combinator "Case" "pattern"
  let guarded = combinator "Case" "guarded"
end

module Exp = struct
  let var = combinator "Exp" "var"
  let ident = combinator "Exp" "ident"
  let constant = combinator "Exp" "constant"
  let local = combinator "Exp" "local"
  let let_nonbinding = combinator "Exp" "let_nonbinding"
  let let_simple = combinator "Exp" "let_simple"
  let let_rec_simple = combinator "Exp" "let_rec_simple"
  let let_pattern = combinator "Exp" "let_pattern"
  let fun_nonbinding = combinator "Exp" "fun_nonbinding"
  let fun_simple = combinator "Exp" "fun_simple"
  let fun_pattern = combinator "Exp" "fun_pattern"
  let function_ = combinator "Exp" "function_"
  let apply = combinator "Exp" "apply"
  let match_ = combinator "Exp" "match_"
  let try_ = combinator "Exp" "try_"
  let tuple = combinator "Exp" "tuple"
  let construct = combinator "Exp" "construct"
  let variant = combinator "Exp" "variant"
  let record = combinator "Exp" "record"
  let field = combinator "Exp" "field"
  let setfield = combinator "Exp" "setfield"
  let array = combinator "Exp" "array"
  let ifthenelse = combinator "Exp" "ifthenelse"
  let sequence = combinator "Exp" "sequence"
  let while_ = combinator "Exp" "while_"
  let for_ = combinator "Exp" "for_"
  let send = combinator "Exp" "send"
  let assert_ = combinator "Exp" "assert_"
  let lazy_ = combinator "Exp" "lazy_"
  (*
  let open_ = combinator "Exp" "open_"
  *)
  let quote = combinator "Exp" "quote"
  let escape = combinator "Exp" "escape"
  let to_closed = combinator "Exp" "to_closed"
end

let use comb =
  Lazy.force comb

let apply loc comb args =
  let comb = Lazy.force comb in
  Lapply
  { ap_func = comb;
    ap_args = args;
    ap_loc = loc;
    ap_should_be_tailcall = false;
    ap_inlined = Default_inline;
    ap_specialised = Default_specialise;
  }

let string s =
  Lconst (Const_base (Const_string(s, None)))

let marshal_loc (x : Location.t) =
  let s = Marshal.to_string x [] in
    string s

let marshal_name (x : string loc) =
  let s = Marshal.to_string x [] in
    string s

let marshal_constant (x : CamlinternalAST.constant) =
  let s = Marshal.to_string x [] in
    string s

let marshal_ident (x : Longident.t loc) =
  let s = Marshal.to_string x [] in
    string s

let marshal_arg_label (x : Asttypes.arg_label) =
  let s = Marshal.to_string x [] in
    string s

let true_ = Lconst(Const_pointer 1)

let false_ = Lconst(Const_pointer 0)

let none =  Lconst(Const_pointer 0)

let some x = Lprim(Pmakeblock(0, Immutable, None), [x], Location.none)

let option opt =
  match opt with
  | None -> none
  | Some x -> some x

let nil = Lconst(Const_pointer 0)

let cons hd tl = Lprim(Pmakeblock(0, Immutable, None), [hd; tl], Location.none)

let rec list l =
  match l with
  | [] -> nil
  | hd :: tl -> cons hd (list tl)

let pair (x, y) =
  Lprim(Pmakeblock(0, Immutable, None), [x; y], Location.none)

let triple (x, y, z) =
  Lprim(Pmakeblock(0, Immutable, None), [x; y; z], Location.none)

let func id body =
  Lfunction {
    kind = Curried;
    params = [id];
    body = body;
    attr = default_function_attribute;
    loc = Location.none;
  }

let list_func ids body =
  let rec loop list_id = function
    | [] -> body
    | [id] ->
        Llet(Alias, Pgenval, id,
             Lprim(Pfield 0, [Lvar list_id], Location.none),
             body)
    | id :: ids ->
        let tail_id = Ident.create "tl" in
        Llet(Alias, Pgenval, id,
          Lprim(Pfield 0, [Lvar list_id], Location.none),
          Llet(Alias, Pgenval, tail_id,
               Lprim(Pfield 1, [Lvar list_id], Location.none),
               loop tail_id ids))
  in
  let list_id = Ident.create "list" in
  let body = loop list_id ids in
  Lfunction {
    kind = Curried;
    params = [list_id];
    body = body;
    attr = default_function_attribute;
    loc = Location.none;
  }

let bind id def body =
  Llet(Strict, Pgenval, id, def, body)

let quote_loc (loc : Location.t) =
  if loc = Location.none then use Loc.none
  else apply Location.none Loc.unmarshal [marshal_loc loc]

let quote_constant loc (const : Asttypes.constant) =
  let const : CamlinternalAST.constant =
    let open CamlinternalAST in
    match const with
    | Const_int x -> Pconst_integer (string_of_int x, None)
    | Const_char x -> Pconst_char x
    | Const_string (x,y) -> Pconst_string (x,y)
    | Const_float x -> Pconst_float (x, None)
    | Const_int32 x -> Pconst_integer (Int32.to_string x, Some 'l')
    | Const_int64 x -> Pconst_integer (Int64.to_string x, Some 'L')
    | Const_nativeint x -> Pconst_integer (Nativeint.to_string x, Some 'n')
  in
  apply loc Constant.unmarshal [marshal_constant const]

let quote_name loc (str : string loc) =
  apply loc Name.unmarshal [marshal_name str]

let quote_variant loc (variant : label) =
  apply loc Variant.of_string [string variant]

let wrap_local loc id (name : string loc) body =
  let name = quote_name name.loc name in
  apply loc Exp.local [quote_loc loc; name; func id body]

let quote_method loc (meth : Typedtree.meth) =
  let name =
    match meth with
    | Tmeth_name name -> name
    | Tmeth_val id -> Ident.name id
  in
  apply loc Method.of_string [string name]

(*
let quote_label loc = function
  | "" -> use Label.none
  | lbl ->
    apply loc Label.of_string [string lbl]
*)

let quote_arg_label loc (lbl : Asttypes.arg_label) =
  apply loc ArgLabel.unmarshal [marshal_arg_label lbl]

let lid_of_path p =
  let rec loop = function
    | Path.Pident id ->
        if Ident.global id then Longident.Lglobal (Ident.name id)
        else begin
          match
            List.find (fun (_, bid) -> Ident.equal id bid)
                      Predef.builtin_idents
          with
          | name, _ -> Longident.Lglobal name
          | exception Not_found -> raise Exit
        end
    | Path.Pdot(p, s, _) ->
        Longident.Ldot(loop p, s)
    | Path.Papply _ -> raise Exit
  in
    match loop p with
    | lid -> Some lid
    | exception Exit -> None

let lid_of_type_path env ty =
  let desc =
    (Ctype.repr (Ctype.expand_head_opt env (Ctype.correct_levels ty))).desc
  in
  match desc with
  | Tconstr(p, _, _) -> lid_of_path p
  | _ -> None

let quote_variant_constructor env loc constr =
  let lid =
    match lid_of_type_path env constr.cstr_res with
    | None -> fatal_error "No global path for variant constructor"
    | Some (Longident.Lident _) -> Longident.Lident constr.cstr_name
    | Some (Longident.Lglobal _) -> Longident.Lglobal constr.cstr_name
    | Some (Longident.Ldot(lid, _)) -> Longident.Ldot(lid, constr.cstr_name)
    | Some (Longident.Lfrommacro _) -> assert false
    | Some (Longident.Lapply _) -> assert false
  in
  let lid = mkloc lid loc in
  apply loc Identifier.unmarshal [marshal_ident lid]

let quote_record_label env loc lbl =
  let lid =
    match lid_of_type_path env lbl.lbl_res with
    | None -> fatal_error "No global path for record label"
    | Some (Longident.Lident _) -> Longident.Lident lbl.lbl_name
    | Some (Longident.Lglobal _) -> Longident.Lglobal lbl.lbl_name
    | Some (Longident.Ldot(lid, _)) -> Longident.Ldot(lid, lbl.lbl_name)
    | Some (Longident.Lfrommacro _) -> assert false
    | Some (Longident.Lapply _) -> assert false
  in
  let lid = mkloc lid loc in
  apply loc Identifier.unmarshal [marshal_ident lid]

(** [transl_clos_field id i] returns the lambda code constructing
    [Lfrommacro (lid, i)], where [lid] is the contents of the variable referred
    to by [id] ([id] is assumed to be in scope). *)
let transl_clos_field loc path_id index =
  let lid =
    apply loc Identifier.lfrommacro
      [apply loc Identifier.unmarshal [Lvar path_id];
       Lconst (Const_base (Const_int index))
      ]
  in
  apply loc Exp.ident [quote_loc loc; lid]

let rec quote_pattern p =
  let env = p.pat_env in
  let loc = p.pat_loc in
  match p.pat_desc with
  | Tpat_any -> apply loc Pat.any [quote_loc loc]
  | Tpat_var(id, _) -> apply loc Pat.var [quote_loc loc; Lvar id]
  | Tpat_alias(pat, id, _) ->
      let pat = quote_pattern pat in
      apply loc Pat.alias [quote_loc loc; pat; Lvar id]
  | Tpat_constant const ->
      let const = quote_constant loc const in
      apply loc Pat.constant [quote_loc loc; const]
  | Tpat_tuple pats ->
      let pats = List.map quote_pattern pats in
      apply loc Pat.tuple [quote_loc loc; list pats]
  | Tpat_construct(lid, constr, args) ->
      let constr = quote_variant_constructor env lid.loc constr in
      let args =
        match args with
        | [] -> None
        | [arg] ->
            let arg = quote_pattern arg in
            Some arg
        | _ :: _ ->
            let args = List.map quote_pattern args in
            Some (apply loc Pat.tuple [quote_loc loc; list args])
      in
      apply loc Pat.construct [quote_loc loc; constr; option args]
  | Tpat_variant(variant, argo, _) ->
      let variant = quote_variant loc variant in
      let argo = Misc.may_map quote_pattern argo in
      apply loc Pat.variant [quote_loc loc; variant; option argo]
  | Tpat_record(lbl_pats, closed) ->
      let lbl_pats =
        List.map
          (fun (lid, lbl, pat) ->
            let lbl =
              quote_record_label env (lid : Longident.t loc).loc lbl
            in
            let pat = quote_pattern pat in
            pair (lbl, pat))
          lbl_pats
      in
      let closed =
        match closed with
        | Asttypes.Closed -> true_
        | Asttypes.Open -> false_
      in
      apply loc Pat.record [quote_loc loc; list lbl_pats; closed]
  | Tpat_array pats ->
      let pats = List.map quote_pattern pats in
      apply loc Pat.array [quote_loc loc; list pats]
  | Tpat_or(pat1, pat2, _) ->
      let pat1 = quote_pattern pat1 in
      let pat2 = quote_pattern pat2 in
      apply loc Pat.or_ [quote_loc loc; pat1; pat2]
  | Tpat_lazy pat ->
      let pat = quote_pattern pat in
      apply loc Pat.lazy_ [quote_loc loc; pat]

type case_binding =
  | Non_binding of lambda * lambda
  | Simple of lambda * lambda
  | Pattern of lambda * lambda
  | Guarded of lambda * lambda

let rec case_binding exn transl pclos stage case : case_binding =
  let pat = case.c_lhs in
  let pat_loc = pat.pat_loc in
  match case.c_guard with
  | None -> begin
      match pat.pat_desc, exn with
      | Tpat_var(id, name), false ->
          let name = quote_name name.loc name in
          let body = quote_expression transl pclos stage case.c_rhs in
          Simple(name, func id body)
      | _ ->
          match pat_bound_idents pat with
          | [] ->
              let pat = quote_pattern pat in
              let pat =
                if exn then
                  apply pat_loc Pat.exception_ [quote_loc pat_loc; pat]
                else
                  pat
              in
              let exp = quote_expression transl pclos stage case.c_rhs in
              Non_binding(pat, exp)
          | id_names ->
              let ids = List.map fst id_names in
              let names =
                List.map (fun (_, name) ->
                    quote_name (name : string loc).loc name)
                  id_names
              in
              let pat = quote_pattern pat in
              let pat =
                if exn then
                  apply pat_loc Pat.exception_ [quote_loc pat_loc; pat]
                else
                  pat
              in
              let exp = quote_expression transl pclos stage case.c_rhs in
              let pat_id = Ident.create "pattern" in
              let exp_id = Ident.create "expression" in
              let body =
                bind pat_id pat
                  (bind exp_id exp
                    (pair (Lvar pat_id, Lvar exp_id)))
              in
              Pattern(list names, list_func ids body)
    end
  | Some guard ->
      let id_names = pat_bound_idents case.c_lhs in
      let ids = List.map fst id_names in
      let names =
        List.map
          (fun (_, name) -> quote_name (name : string loc).loc name)
          id_names
      in
      let pat = quote_pattern case.c_lhs in
      let pat =
        if exn then apply pat_loc Pat.exception_ [quote_loc pat_loc; pat]
        else pat
      in
      let guard = quote_expression transl pclos stage guard in
      let exp = quote_expression transl pclos stage case.c_rhs in
      let pat_id = Ident.create "pattern" in
      let guard_id = Ident.create "guard" in
      let exp_id = Ident.create "expression" in
      let body =
        bind pat_id pat
          (bind guard_id guard
            (bind exp_id exp
              (triple (Lvar pat_id, Lvar guard_id, Lvar exp_id))))
      in
      Guarded(list names, list_func ids body)

and quote_case_binding loc cb =
  match cb with
  | Non_binding(pat, exp) ->
      apply loc Case.nonbinding [quote_loc loc; pat; exp]
  | Simple(name, body) ->
      apply loc Case.simple [quote_loc loc; name; body]
  | Pattern(names, body) ->
      apply loc Case.pattern [quote_loc loc; names; body]
  | Guarded(names, body) ->
      apply loc Case.guarded [quote_loc loc; names; body]

and quote_case exn transl pclos stage loc case =
  quote_case_binding loc (case_binding exn transl pclos stage case)

and quote_nonrecursive_let transl pclos stage vbs body =
  List.fold_right
    (fun vb body ->
      let loc = vb.vb_loc in
      let pat = vb.vb_pat in
      let exp = vb.vb_expr in
      match pat.pat_desc with
      | Tpat_var(id, name) ->
          let name = quote_name name.loc name in
          let exp = quote_expression transl pclos stage exp in
          apply loc Exp.let_simple
                [quote_loc loc; name; exp; func id body]
      | _ ->
          match pat_bound_idents pat with
          | [] ->
              let pat = quote_pattern pat in
              let exp = quote_expression transl pclos stage exp in
              apply loc Exp.let_nonbinding [quote_loc loc; pat; exp; body]
          | id_names ->
              let ids = List.map fst id_names in
              let names =
                List.map
                  (fun (_, name) -> quote_name (name : string loc).loc name)
                  id_names
              in
              let pat = quote_pattern pat in
              let exp = quote_expression transl pclos stage exp in
              let pat_id = Ident.create "pattern" in
              let body_id = Ident.create "body" in
              let body =
                bind pat_id pat
                  (bind body_id body
                    (pair (Lvar pat_id, Lvar body_id)))
              in
              apply loc Exp.let_pattern
                    [quote_loc loc; list names; exp; list_func ids body])
    vbs (quote_expression transl pclos stage body)

and quote_recursive_let transl pclos stage loc vbs body =
  let id_names, exps =
    List.fold_right
      (fun vb (id_names, exps) ->
        let pat = vb.vb_pat in
        let exp = vb.vb_expr in
        match pat.pat_desc with
        | Tpat_var(id, name)
        | Tpat_alias ({pat_desc=Tpat_any}, id, name) ->
            let name = quote_name name.loc name in
            let exp = quote_expression transl pclos stage exp in
            ((id, name) :: id_names, exp :: exps)
        | _ -> assert false)
      vbs ([], [])
  in
  let ids = List.map fst id_names in
  let names = List.map snd id_names in
  let exps_id = Ident.create "exps" in
  let body_id = Ident.create "body" in
  let body =
    bind exps_id (list exps)
      (bind body_id (quote_expression transl pclos stage body)
        (pair (Lvar exps_id, Lvar body_id)))
  in
  apply loc Exp.let_rec_simple
        [quote_loc loc; list names; list_func ids body]

and quote_expression transl pclos stage e =
  let env = e.exp_env in
  let loc = e.exp_loc in
  match e.exp_desc with
  | Texp_ident(path, _, _) ->
    begin
      let quote_path path =
        match lid_of_path path with
        | Some lid ->
            let lid = mkloc lid loc in
            let lid = apply loc Identifier.unmarshal [marshal_ident lid] in
            apply loc Exp.ident [quote_loc loc; lid]
        | None ->
            match path with
            | Path.Pident id ->
                apply loc Exp.var [quote_loc loc; Lvar id]
            | Path.Pdot _ | Path.Papply _ ->
                fatal_error "No global path for identifier"
      in
      match pclos with
      | None -> quote_path path
      | Some (path_id, map) ->
          try
            let field_idx = Env.PathMap.find path map in
            transl_clos_field loc path_id field_idx
          with Not_found ->
            quote_path path
    end
  | Texp_constant const ->
      let const = quote_constant loc const in
      apply loc Exp.constant [quote_loc loc; const]
  | Texp_let(rf, vbs, body) -> begin
      match rf with
      | Nonrecursive ->
          quote_nonrecursive_let transl pclos stage vbs body
      | Recursive ->
          quote_recursive_let transl pclos stage loc vbs body
    end
  | Texp_function(label, cases, _) -> begin
      let cbs = List.map (case_binding false transl pclos stage) cases in
      match cbs with
      | [Non_binding(pat, exp)] ->
          let label = quote_arg_label loc label in
          apply loc Exp.fun_nonbinding [quote_loc loc; label; pat; exp]
      | [Simple(name, body)] ->
          let label = quote_arg_label loc label in
          apply loc Exp.fun_simple [quote_loc loc; name; label; none; body]
      | [Pattern(names, body)] ->
          let label = quote_arg_label loc label in
          apply loc Exp.fun_pattern [quote_loc loc; names; label; none; body]
      | cases ->
          let cases = List.map (quote_case_binding loc) cases in
          apply loc Exp.function_ [quote_loc loc; list cases]
    end
  | Texp_apply(fn, args) ->
      let fn = quote_expression transl pclos stage fn in
      let args = List.filter (fun (_, exp) -> exp <> None) args in
      let args =
        List.map
          (fun (lbl, exp) ->
             match exp with
             | None -> assert false
             | Some exp ->
                 let lbl = quote_arg_label loc lbl in
                 let exp = quote_expression transl pclos stage exp in
                   pair (lbl, exp))
          args
      in
      apply loc Exp.apply [quote_loc loc; fn; list args]
  | Texp_match(exp, cases, exn_cases, _) ->
      let exp = quote_expression transl pclos stage exp in
      let cases = List.map (quote_case false transl pclos stage loc) cases in
      let exn_cases =
        List.map (quote_case true transl pclos stage loc) exn_cases
      in
      apply loc Exp.match_ [quote_loc loc; exp; list (cases @ exn_cases)]
  | Texp_try(exp, cases) ->
      let exp = quote_expression transl pclos stage exp in
      let cases = List.map (quote_case false transl pclos stage loc) cases in
      apply loc Exp.try_ [quote_loc loc; exp; list cases]
  | Texp_tuple exps ->
      let exps = List.map (quote_expression transl pclos stage) exps in
      apply loc Exp.tuple [quote_loc loc; list exps]
  | Texp_construct(lid, constr, args) ->
      let constr = quote_variant_constructor env lid.loc constr in
      let args =
        match args with
        | [] -> None
        | [arg] ->
            let arg = quote_expression transl pclos stage arg in
            Some arg
        | _ :: _ ->
            let args = List.map (quote_expression transl pclos stage) args in
            Some (apply loc Exp.tuple [quote_loc loc; list args])
      in
      apply loc Exp.construct [quote_loc loc; constr; option args]
  | Texp_variant(variant, argo) ->
      let variant = quote_variant loc variant in
      let argo = Misc.may_map (quote_expression transl pclos stage) argo in
      apply loc Exp.variant [quote_loc loc; variant; option argo]
  | Texp_record { fields=fields; extended_expression=base } ->
      let lbl_exps =
        List.map
          (function
           | (lbl, (Overridden (lid, exp))) ->
             let lbl = quote_record_label env (lid : Longident.t loc).loc lbl in
             let exp = quote_expression transl pclos stage exp in
            pair (lbl, exp)
           | _ -> assert false (* unused *)
          ) @@
        List.filter
          (function
           | (_, (Overridden _)) -> true
           | _ -> false) @@
        Array.to_list fields
      in
      let base = Misc.may_map (quote_expression transl pclos stage) base in
      apply loc Exp.record [quote_loc loc; list lbl_exps; option base]
  | Texp_field(rcd, lid, lbl) ->
      let rcd = quote_expression transl pclos stage rcd in
      let lbl = quote_record_label env lid.loc lbl in
      apply loc Exp.field [quote_loc loc; rcd; lbl]
  | Texp_setfield(rcd, lid, lbl, exp) ->
      let rcd = quote_expression transl pclos stage rcd in
      let lbl = quote_record_label env lid.loc lbl in
      let exp = quote_expression transl pclos stage exp in
      apply loc Exp.setfield [quote_loc loc; rcd; lbl; exp]
  | Texp_array exps ->
      let exps = List.map (quote_expression transl pclos stage) exps in
      apply loc Exp.array [quote_loc loc; list exps]
  | Texp_ifthenelse(cond, then_, else_) ->
      let cond = quote_expression transl pclos stage cond in
      let then_ = quote_expression transl pclos stage then_ in
      let else_ = Misc.may_map (quote_expression transl pclos stage) else_ in
      apply loc Exp.ifthenelse [quote_loc loc; cond; then_; option else_]
  | Texp_sequence(exp1, exp2) ->
      let exp1 = quote_expression transl pclos stage exp1 in
      let exp2 = quote_expression transl pclos stage exp2 in
      apply loc Exp.sequence [quote_loc loc; exp1; exp2]
  | Texp_while(cond, body) ->
      let cond = quote_expression transl pclos stage cond in
      let body = quote_expression transl pclos stage body in
      apply loc Exp.while_ [quote_loc loc; cond; body]
  | Texp_for(id, pat, low, high, dir, body) ->
      let low = quote_expression transl pclos stage low in
      let high = quote_expression transl pclos stage high in
      let dir =
        match dir with
        | Asttypes.Upto -> true_
        | Asttypes.Downto -> false_
      in
      let name =
        match pat.Parsetree.ppat_desc with
        | Parsetree.Ppat_var name -> name
        | Parsetree.Ppat_any ->
            Location.mkloc "_" pat.Parsetree.ppat_loc
        | _ -> assert false
      in
      let name = quote_name name.loc name in
      let body = quote_expression transl pclos stage body in
      apply loc Exp.for_
            [quote_loc loc; name; low; high; dir; func id body]
  | Texp_send(obj, meth, _) ->
      let obj = quote_expression transl pclos stage obj in
      let meth = quote_method loc meth in
      apply loc Exp.send [quote_loc loc; obj; meth]
  | Texp_assert exp ->
      let exp = quote_expression transl pclos stage exp in
      apply loc Exp.assert_ [quote_loc loc; exp]
  | Texp_lazy exp ->
      let exp = quote_expression transl pclos stage exp in
      apply loc Exp.lazy_ [quote_loc loc; exp]
  | Texp_quote exp ->
      let exp = quote_expression transl pclos (stage + 1) exp in
      apply loc Exp.quote [quote_loc loc; exp]
  | Texp_escape exp ->
      if stage > 0 then begin
        let exp = quote_expression transl pclos (stage - 1) exp in
        apply loc Exp.escape [quote_loc loc; exp]
      end else transl exp
  | Texp_new _ | Texp_instvar _ | Texp_setinstvar _ | Texp_override _
  | Texp_letmodule _ | Texp_object _ | Texp_pack _ | Texp_unreachable
  | Texp_letexception _ | Texp_extension_constructor _ ->
      fatal_error "Expression cannot be quoted"

let quote_expression transl pclos exp =
  quote_expression transl pclos 0 exp

let transl_close_expression loc lam =
  apply loc Exp.to_closed [lam]
