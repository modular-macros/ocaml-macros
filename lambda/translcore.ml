(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Misc
open Asttypes
open Primitive
open Types
open Data_types
open Typedtree
open Typeopt
open Lambda
open Debuginfo.Scoped_location

type error =
    Free_super_var
  | Unreachable_reached
  | Objects_in_staged_code

exception Error of Location.t * error

(* Objects and classes are not supported in quotations or in
   compile-time code. *)
let check_no_objects ?(anywhere = false) (e : Typedtree.expression) =
  let staged env =
    anywhere || Env.get_env_mode env = Types.M_Q || Env.get_env_level env < 0
  in
  let expr iter (e : Typedtree.expression) =
    (match e.exp_desc with
     | Texp_object _ | Texp_new _ | Texp_instvar _
     | Texp_setinstvar _ | Texp_override _ when staged e.exp_env ->
         raise (Error (e.exp_loc, Objects_in_staged_code))
     | _ -> ());
    Tast_iterator.default_iterator.expr iter e
  in
  let structure_item iter (item : Typedtree.structure_item) =
    (match item.str_desc with
     | Tstr_class _ when staged item.str_env ->
         raise (Error (item.str_loc, Objects_in_staged_code))
     | _ -> ());
    Tast_iterator.default_iterator.structure_item iter item
  in
  let iter = { Tast_iterator.default_iterator with expr; structure_item } in
  iter.expr iter e

let use_dup_for_constant_mutable_arrays_bigger_than = 4

(* The state of a translation *)

(* The part of the unit that the translation is building *)
type world =
  | Run_world      (* the object that the program runs *)
  | Compile_world  (* the compile-time part *)

(* The code that a generator builds refers to the run-time values of
   its unit through an environment: a block of those values, held in
   the run-time slot of the generator, which each call of the generator
   passes as code.  While the body of a generator is translated,
   [macro_env] gives the parameter that receives the environment and
   the position of each captured identifier in it; a quoted reference
   to such an identifier becomes a projection from the environment. *)
type macro_env =
  { me_param : Ident.t;                 (* the environment parameter *)
    me_index : (Ident.t * int list) list;
    (* The fields to project, outermost first, for each captured
       identifier *)
    me_group : Ident.Set.t }
    (* The generators of the group, which share the environment *)

(* The closure backend translates a unit in the store form: its
   components are fields of the block of the unit, and the scope of a
   local binding does not span the items that follow.  Quoted code
   therefore names a component by its field.  [nsl_pos] gives the field
   of each identifier of the unit. *)
type native_slot_layout =
  { nsl_module : Ident.t;
    nsl_pos : Ident.t -> int option }

(* Where template applications are compiled, and how: among the
   items of a static program; under a functor body, whose applications
   are hoisted to the functor's definition item; under a template
   body, whose applications have their own routing
   ([template_body_apps]). *)
type template_apps_mode =
  | Tapps_inactive       (* no static program at all *)
  | Tapps_items          (* the static program's own structure items *)
  | Tapps_plain_body     (* under a functor's body, within a
                            static program's translation *)
  | Tapps_template_body  (* under a template's body *)
  | Tapps_template_plain_body
                         (* under a functor's body that is
                            itself under a template's body *)

(* The identifiers of an application, [module M = F$(V)], which the
   passes of the unit share; see [Translmod]. *)
type template_application_idents =
  { ta_mcode : Ident.t;   (* the fragment code: the static-body pass
                             binds it, the run-time term's hole reads it *)
    ta_arg : Ident.t;     (* in-item binder for a non-path argument *)
    ta_funct : Ident.t }  (* in-item binder for a non-path functor *)

(* An application inside a template body is coordinated between the
   body's two passes within one [transl_template_body]: the run-world
   pass leaves a marker hole at M0's position and collects the
   application with the run-world term of its environment pair; the
   compile-world pass binds the component call's pair, M0's record its
   first field, and a thunk applies the pair's dynamic half at the
   marker, once per outer application. *)
type template_body_app =
  { tba_m_id : Ident.t;               (* M0 *)
    tba_hole : Ident.t;               (* the fragment's marker hole *)
    tba_pair : Ident.t;               (* the chain's (record, dyn) pair *)
    tba_pair_term : lambda;           (* run-world (G slot, coerced A block) *)
    tba_lit_funct : Ident.t option;   (* in-item binder, literal functor *)
    tba_lit_arg : Ident.t option }    (* in-item binder, literal argument *)

(* What the passes of the translation of a unit record, and read *)
type unit_state =
  { mutable compile_world_dummies : Ident.Set.t;
    (* The identifiers that the compile-time part binds to
       placeholders: the run-time components, which keep only their
       positions in the compile-time block. *)
    mutable hidden_application_binders : Ident.Set.t;
    (* The binders of the hidden application items that template
       applications inside module expressions compile as *)
    mutable template_applications :
      template_application_idents Ident.Map.t;
    (* The identifiers of each application, by the identifier of its
       module *) }

type context =
  { world : world;
    single_block : bool;
    (* Whether the module being translated is the value of an
       expression: a pack, a [let module], a [let open struct .. end].
       Such a module is built as one block, in the part that evaluates
       the expression, with no placeholders; only a unit's structure and
       the modules named in it have a run-time block and a compile-time
       block.  The items of such a module are translated as in the
       run-time part, while a path names the block of the part that the
       expression is really in. *)
    quotation_depth : int;
    (* The depth of quotations around the expression being translated,
       less that of the splices inside them: quoted code is run-time
       code. *)
    macro_env : macro_env option;
    (* The environment of the generator whose body is being translated *)
    native_slot_layout : native_slot_layout option;
    (* The fields of the unit's block, in the compile-time part of a
       native static program *)
    template_slot_index : (Ident.t * Ident.t) list option;
    (* The environment slots of a template body's generators, called
       from the body's splices, are fragment binders, freshened at each
       instantiation: an access is routed through a code parameter that
       the thunk binds from the identity delivered at its hole. *)
    splice_source : (context -> expression -> lambda_splice) option;
    (* How the hole of a top-level splice is filled when the run-time
       term is translated: the function gives the contents of the hole,
       in the context of the splice. *)
    closing : Ident.t Ident.Map.t;
    (* The renaming of the run-time variables that the code of a
       top-level splice quotes; see [Translquote.transl_close_function]. *)
    quoted_variables : Ident.Set.t ref option;
    (* Where the quotations of the compile-time code being translated
       add the variables of run-time code that they mention: the free
       variables of their translated bodies, a splice in a body counting
       as its arguments *)
    unit_globals : Ident.t -> Ident.t;
    (* The global of a unit that compile-time code names: in the
       toplevel, a unit loaded for compile-time code has a name of its
       own *)
    template_apps_mode : template_apps_mode;
    translating_macros_object : bool;
    (* Whether the compile-time object's term is being translated,
       rather than the static program's body: an application's M is a
       record call in the former, the evaluated fragment in the
       latter. *)
    template_body_apps : template_body_app list ref option;
    (* The applications of the template body being translated *)
    template_body_pending_chain : (lambda -> lambda) list ref option;
    (* The pair bindings of applications inside a functor body nested
       under a template body: a let cannot span the functor's lambda and
       the dynamic component, so each is bound outside both, at the
       plain body's definition item (or at the top of the component
       function for a body that is a bare functor), once per
       instantiation, in declaration order.  Each entry wraps the term
       downstream of it. *)
    innermost_free_variables : Ident.Set.t;
    (* The free variables of the code at the innermost point of the
       compile-time part, which fills the holes of the run-time term:
       each must be in scope there. *)
    unit_state : unit_state }

let initial_context () =
  { world = Run_world;
    single_block = false;
    quotation_depth = 0;
    macro_env = None;
    native_slot_layout = None;
    template_slot_index = None;
    splice_source = None;
    closing = Ident.Map.empty;
    quoted_variables = None;
    unit_globals = Fun.id;
    template_apps_mode = Tapps_inactive;
    translating_macros_object = false;
    template_body_apps = None;
    template_body_pending_chain = None;
    innermost_free_variables = Ident.Set.empty;
    unit_state =
      { compile_world_dummies = Ident.Set.empty;
        hidden_application_binders = Ident.Set.empty;
        template_applications = Ident.Map.empty } }

let in_quotation ctx = ctx.quotation_depth > 0

(* The globals of the units that compile-time code names, renamed *)
let rename_unit_globals ctx lam =
  Lambda.map
    (function
      | Lprim (Pgetglobal id, args, loc) ->
          Lprim (Pgetglobal (ctx.unit_globals id), args, loc)
      | lam -> lam)
    lam

let rec name_slots_through_block nsl lam =
  match lam with
  | Lvar r ->
      begin match nsl.nsl_pos r with
      | Some pos ->
          Lprim (Pfield (pos, Pointer, Immutable),
                 [Lprim (Pgetglobal nsl.nsl_module, [], Loc_unknown)],
                 Loc_unknown)
      | None -> lam
      end
  | Lsplice _ ->
      lam
  | _ -> Lambda.shallow_map (name_slots_through_block nsl) lam

let captured_index me h =
  List.find_map
    (fun (h', i) -> if Ident.same h h' then Some i else None)
    me.me_index

(* Project the captured identifiers of quoted code from the
   environment.  The code of a splice is compile-time code, and is left
   alone. *)
let rec project_captured me lam =
  match lam with
  | Lvar h ->
      begin match captured_index me h with
      | Some path ->
          List.fold_right
            (fun i acc ->
               Lprim (Pfield (i, Pointer, Immutable), [acc], Loc_unknown))
            path
            (lsplice (Lvar me.me_param))
      | None -> lam
      end
  | Lsplice _ -> lam
  | _ -> Lambda.shallow_map (project_captured me) lam

let rec route_template_slots idx lam =
  match lam with
  | Lvar r ->
      (match
         List.find_map
           (fun (r', c) -> if Ident.same r r' then Some c else None) idx
       with
       | Some code_param -> lsplice (Lvar code_param)
       | None -> lam)
  | Lsplice _ -> lam
  | _ -> Lambda.shallow_map (route_template_slots idx) lam

(* Forward declaration -- to be filled in by Translmod.transl_module *)
let transl_module =
  ref((fun ~ctx:_ ~scopes:_ _cc _rootpath _modl -> assert false) :
      ctx:context -> scopes:scopes -> module_coercion -> Path.t option ->
      module_expr -> lambda)

(* The splice in a quotation of the given code, closed over [vars],
   the variables of the quoted term that the quotations in the code
   mention: a function of them.  They occur in the splice as its
   arguments only, where they follow the quoted term through renaming.
   The other free variables of the code are those of compile-time
   code. *)
let close_splice loc vars body =
  match body, Ident.Set.elements vars with
  | Lvar _, _ | _, [] -> { sp_code = body; sp_args = [] }
  | _, vars ->
      let params = List.map (fun v -> v, Ident.rename v) vars in
      let renaming =
        List.fold_left (fun m (v, p) -> Ident.Map.add v p m)
          Ident.Map.empty params
      in
      { sp_code =
          lfunction ~kind:Curried
            ~params:(List.map (fun (_, p) -> p, Pgenval) params)
            ~return:Pgenval ~attr:default_function_attribute ~loc
            ~body:(rename renaming body);
        sp_args = vars }

let transl_struct_item =
  ref ((fun ~ctx:_ ~scopes:_ _fields _rootpath _stri _next -> assert false) :
       ctx:context -> scopes:scopes -> Ident.t list -> Path.t option ->
       structure_item -> (Ident.t list -> lambda) -> lambda)

let transl_object =
  ref (fun ~ctx:_ ~scopes:_ _id _s _cl -> assert false :
       ctx:context -> scopes:scopes -> Ident.t -> string list -> class_expr ->
       lambda)

(* Compile an exception/extension definition *)

let prim_fresh_oo_id =
  Pccall (Primitive.simple ~name:"caml_fresh_oo_id" ~arity:1 ~alloc:false)

let transl_extension_constructor ~scopes env path ext =
  let path =
    Printtyp.wrap_printing_env env ~error:true (fun () ->
      Option.map (Out_type.rewrite_double_underscore_paths env) path)
  in
  let name =
    match path, !Clflags.for_package with
      None, _ -> Ident.name ext.ext_id
    | Some p, None -> Path.name p
    | Some p, Some pack -> Printf.sprintf "%s.%s" pack (Path.name p)
  in
  let loc = of_location ~scopes ext.ext_loc in
  match ext.ext_kind with
    Text_decl _ ->
      Lprim (Pmakeblock (Obj.object_tag, Immutable, None),
        [Lconst (Const_immstring name);
         Lprim (prim_fresh_oo_id, [Lconst (const_int 0)], loc)],
        loc)
  | Text_rebind(path, _lid) ->
      transl_extension_path loc env path

(* To propagate structured constants *)

exception Not_constant

let extract_constant = function
    Lconst sc -> sc
  | _ -> raise Not_constant

let extract_float = function
    Const_float f -> f
  | _ -> fatal_error "Translcore.extract_float"

(* Insertion of debugging events *)

let event_before ~scopes exp lam =
  Translprim.event_before (of_location ~scopes exp.exp_loc) exp lam

let event_after ~scopes exp lam =
  Translprim.event_after (of_location ~scopes exp.exp_loc) exp lam

let event_function ~scopes exp lam =
  if !Clflags.debug && not !Clflags.native_code then
    let repr = Some (ref 0) in
    let (info, body) = lam repr in
    (info,
     Levent(body, {lev_loc = of_location ~scopes exp.exp_loc;
                   lev_kind = Lev_function;
                   lev_repr = repr;
                   lev_env = exp.exp_env}))
  else
    lam None

(* Assertions *)

let assert_failed loc ~scopes exp =
  let slot =
    transl_extension_path Loc_unknown
      Env.initial Predef.path_assert_failure
  in
  let (fname, line, char) =
    Location.get_pos_info loc.Location.loc_start
  in
  let loc = of_location ~scopes exp.exp_loc in
  Lprim(Praise Raise_regular, [event_after ~scopes exp
    (Lprim(Pmakeblock(0, Immutable, None),
          [slot;
           Lconst(Const_block(0,
              [Const_immstring fname;
               Const_int line;
               Const_int char]))], loc))], loc)

(* In cases where we're careful to preserve syntactic arity, we disable
   the arity fusion attempted by simplif.ml *)
let function_attribute_disallowing_arity_fusion =
  { default_function_attribute with may_fuse_arity = false }

let rec cut n l =
  if n = 0 then ([],l) else
  match l with [] -> failwith "Translcore.cut"
  | a::l -> let (l1,l2) = cut (n-1) l in (a::l1,l2)

(* [fuse_method_arity] is what ensures that a n-ary method is compiled as a
   (n+1)-ary function, where the first parameter is self. It fuses together the
   self and method parameters.

   Input:  fun self -> fun method_param_1 ... method_param_n -> body
   Output: fun self method_param_1 ... method_param_n -> body

   It detects whether the AST is a method by the presence of [Texp_poly] on the
   inner function. This is only ever added to methods.
*)
let fuse_method_arity parent_params parent_body =
  match parent_body with
  | Tfunction_body
      { exp_desc = Texp_function (method_params, method_body);
        exp_extra;
      }
      when
        List.exists
          (function (Texp_poly _, _, _) -> true | _ -> false)
          exp_extra
    -> parent_params @ method_params, method_body
  | _ -> parent_params, parent_body

(* Translation of expressions *)

let rec iter_exn_names f pat =
  match pat.pat_desc with
  | Tpat_var (id, _, _) -> f id
  | Tpat_alias (p, id, _, _, _) ->
      f id;
      iter_exn_names f p
  | _ -> ()

let transl_ident ~ctx loc env ty path desc =
  match desc.val_kind with
  | Val_prim p ->
      Translprim.transl_primitive loc p env ty (Some path)
  | Val_anc _ ->
      raise(Error(to_location loc, Free_super_var))
  | Val_reg | Val_self _ when desc.val_macro = Macro ->
      (* A generator is its compile-time function applied to its
         environment, as code. *)
      let func = transl_value_path loc env path in
      let env_arg =
        match ctx.macro_env, (path : Path.t) with
        | Some me, Pident id when Ident.Set.mem id me.me_group ->
            (* The generators of a group share one environment. *)
            Lvar me.me_param
        | (Some _ | None), _ ->
            let access = transl_value_env_path Loc_unknown env path in
            (* The access is built where the generator is called, in
               compile-time code. *)
            let access =
              if ctx.world = Compile_world && not (in_quotation ctx)
              then rename_unit_globals ctx access
              else access
            in
            let access =
              match ctx.macro_env with
              | Some me -> project_captured me access
              | None -> access
            in
            let access =
              match ctx.template_slot_index with
              | Some idx -> route_template_slots idx access
              | None -> access
            in
            let access =
              match ctx.native_slot_layout with
              | Some nsl -> name_slots_through_block nsl access
              | None -> access
            in
            Translquote.quote_access access
      in
      Lapply { ap_func = func; ap_args = [env_arg]; ap_loc = loc;
               ap_tailcall = Default_tailcall;
               ap_inlined = Default_inline;
               ap_specialised = Default_specialise }
  | Val_reg | Val_self _ ->
      transl_value_path loc env path
  |  _ -> fatal_error "Translcore.transl_exp: bad Texp_ident"

let is_omitted = function
  | Arg _ -> false
  | Omitted () -> true

let rec transl_exp ~ctx ~scopes e =
  transl_exp1 ~ctx ~scopes ~in_new_scope:false e

(* ~in_new_scope tracks whether we just opened a new scope.

   When we just opened a new scope, we avoid introducing an extraneous anonymous
   function scope and instead inherit the new scope. E.g., [let f x = ...] is
   parsed as a let-bound Pexp_function node [let f = fun x -> ...].
   We give it f's scope.
*)
and transl_exp1 ~ctx ~scopes ~in_new_scope e =
  let eval_once =
    (* Whether classes for immediate objects must be cached *)
    match e.exp_desc with
      Texp_function _ | Texp_for _ | Texp_while _ -> false
    | _ -> true
  in
  if eval_once then transl_exp0 ~ctx ~scopes ~in_new_scope  e else
  Translobj.oo_wrap e.exp_env true (transl_exp0 ~ctx ~scopes ~in_new_scope) e

and transl_exp0 ~in_new_scope ~ctx ~scopes e =
  match e.exp_desc with
  | Texp_ident(path, _, desc) ->
      transl_ident ~ctx (of_location ~scopes e.exp_loc)
        e.exp_env e.exp_type path desc
  | Texp_constant cst ->
      Lambda.lambda_of_const cst
  | Texp_let(rec_flag, pat_expr_list, body) ->
      transl_let ~ctx ~scopes rec_flag pat_expr_list
        (event_before ~scopes body (transl_exp ~ctx ~scopes body))
  | Texp_function (params, body) ->
      let scopes =
        if in_new_scope then scopes
        else enter_anonymous_function ~scopes
      in
      transl_function ~ctx ~scopes e params body
  | Texp_apply({ exp_desc = Texp_ident(path, _, {val_kind = Val_prim p});
                exp_type = prim_type } as funct, oargs)
    when List.length oargs >= p.prim_arity
    && List.for_all (fun (_, arg) -> not (is_omitted arg)) oargs ->
      let argl, extra_args = cut p.prim_arity oargs in
      let arg_exps =
         List.map (function _, Arg x -> x | _, Omitted () -> assert false) argl
      in
      let args = transl_list ~ctx ~scopes arg_exps in
      let prim_exp = if extra_args = [] then Some e else None in
      let lam =
        Translprim.transl_primitive_application
          (of_location ~scopes e.exp_loc) p e.exp_env prim_type path
          prim_exp args arg_exps
      in
      if extra_args = [] then lam
      else begin
        let tailcall = Translattribute.get_tailcall_attribute funct in
        let inlined = Translattribute.get_inlined_attribute funct in
        let specialised = Translattribute.get_specialised_attribute funct in
        let e = { e with exp_desc = Texp_apply(funct, oargs) } in
        event_after ~scopes e
          (transl_apply ~ctx ~scopes ~tailcall ~inlined ~specialised
             lam extra_args (of_location ~scopes e.exp_loc))
      end
  | Texp_apply(funct, oargs) ->
      let tailcall = Translattribute.get_tailcall_attribute funct in
      let inlined = Translattribute.get_inlined_attribute funct in
      let specialised = Translattribute.get_specialised_attribute funct in
      let e = { e with exp_desc = Texp_apply(funct, oargs) } in
      event_after ~scopes e
        (transl_apply ~ctx ~scopes ~tailcall ~inlined ~specialised
           (transl_exp ~ctx ~scopes funct) oargs
           (of_location ~scopes e.exp_loc))
  | Texp_match(arg, pat_expr_list, [], partial) ->
      transl_match ~ctx ~scopes e arg pat_expr_list partial
  | Texp_match(arg, pat_expr_list, eff_pat_expr_list, partial) ->
  (* need to separate the values from exceptions for transl_handler *)
      let split_case (val_cases, exn_cases as acc)
            ({ c_lhs; c_rhs } as case) =
        if c_rhs.exp_desc = Texp_unreachable then acc else
        let val_pat, exn_pat = split_pattern c_lhs in
        match val_pat, exn_pat with
        | None, None -> assert false
        | Some pv, None ->
            { case with c_lhs = pv } :: val_cases, exn_cases
        | None, Some pe ->
            val_cases, { case with c_lhs = pe } :: exn_cases
        | Some pv, Some pe ->
            { case with c_lhs = pv } :: val_cases,
            { case with c_lhs = pe } :: exn_cases
      in
      let pat_expr_list, exn_pat_expr_list =
        let x, y = List.fold_left split_case ([], []) pat_expr_list in
        List.rev x, List.rev y
      in
      transl_handler ~ctx ~scopes e arg (Some (pat_expr_list, partial))
        exn_pat_expr_list eff_pat_expr_list
  | Texp_try(body, pat_expr_list, []) ->
      let id = Typecore.name_cases "exn" pat_expr_list in
      Ltrywith(transl_exp ~ctx ~scopes body, id,
               Matching.for_trywith ~scopes e.exp_loc (Lvar id)
                 (transl_cases_try ~ctx ~scopes pat_expr_list))
  | Texp_try(body, exn_pat_expr_list, eff_pat_expr_list) ->
      transl_handler ~ctx ~scopes e body None exn_pat_expr_list
        eff_pat_expr_list
  | Texp_tuple el ->
      let ll, shape = transl_list_with_shape ~ctx ~scopes (List.map snd el) in
      begin try
        Lconst(Const_block(0, List.map extract_constant ll))
      with Not_constant ->
        Lprim(Pmakeblock(0, Immutable, Some shape), ll,
              (of_location ~scopes e.exp_loc))
      end
  | Texp_construct(_, cstr, args) ->
      let ll, shape = transl_list_with_shape ~ctx ~scopes args in
      if cstr.cstr_inlined <> None then begin match ll with
        | [x] -> x
        | _ -> assert false
      end else begin match cstr.cstr_tag with
        Cstr_constant n ->
          Lconst(const_int n)
      | Cstr_unboxed ->
          (match ll with [v] -> v | _ -> assert false)
      | Cstr_block n ->
          begin try
            Lconst(Const_block(n, List.map extract_constant ll))
          with Not_constant ->
            Lprim(Pmakeblock(n, Immutable, Some shape), ll,
                  of_location ~scopes e.exp_loc)
          end
      | Cstr_extension(path, is_const) ->
          let lam = transl_extension_path
                      (of_location ~scopes e.exp_loc) e.exp_env path in
          if is_const then lam
          else
            Lprim(Pmakeblock(0, Immutable, Some (Pgenval :: shape)),
                  lam :: ll, of_location ~scopes e.exp_loc)
      end
  | Texp_extension_constructor (_, path) ->
      transl_extension_path (of_location ~scopes e.exp_loc) e.exp_env path
  | Texp_variant(l, arg) ->
      let tag = Btype.hash_variant l in
      begin match arg with
        None -> Lconst(const_int tag)
      | Some arg ->
          let lam = transl_exp ~ctx ~scopes arg in
          try
            Lconst(Const_block(0, [const_int tag;
                                   extract_constant lam]))
          with Not_constant ->
            Lprim(Pmakeblock(0, Immutable, None),
                  [Lconst(const_int tag); lam],
                  of_location ~scopes e.exp_loc)
      end
  | Texp_record {fields; representation; extended_expression} ->
      transl_record ~ctx ~scopes e.exp_loc e.exp_env
        fields representation extended_expression
  | Texp_atomic_loc (arg, _, lbl) ->
      let shape = Some [Typeopt.value_kind arg.exp_env arg.exp_type; Pintval] in
      let (arg, lbl) = transl_atomic_loc ~ctx ~scopes arg lbl in
      let loc = of_location ~scopes e.exp_loc in
      Lprim (Pmakeblock (0, Immutable, shape), [arg; lbl], loc)
  | Texp_field (arg, _, ({ lbl_atomic = Atomic; _ } as lbl)) ->
      let arg, lbl = transl_atomic_loc ~ctx ~scopes arg lbl in
      let loc = of_location ~scopes e.exp_loc in
      Lprim (Patomic_load, [arg; lbl], loc)
  | Texp_field (arg, _, lbl) ->
      let targ = transl_exp ~ctx ~scopes arg in
      begin match lbl.lbl_repres with
          Record_regular | Record_inlined _ ->
          Lprim (Pfield (lbl.lbl_pos, maybe_pointer e, lbl.lbl_mut), [targ],
                 of_location ~scopes e.exp_loc)
        | Record_unboxed _ -> targ
        | Record_float ->
          Lprim (Pfloatfield lbl.lbl_pos, [targ],
                 of_location ~scopes e.exp_loc)
        | Record_extension _ ->
          Lprim (Pfield (lbl.lbl_pos + 1, maybe_pointer e, lbl.lbl_mut), [targ],
                 of_location ~scopes e.exp_loc)
      end
  | Texp_setfield (arg, _, ({ lbl_atomic = Atomic; _ } as lbl), newval) ->
      let prim =
        Primitive.simple
          ~name:"caml_atomic_exchange_field" ~arity:3 ~alloc:false
      in
      let arg, lbl = transl_atomic_loc ~ctx ~scopes arg lbl in
      let newval = transl_exp ~ctx ~scopes newval in
      let loc = of_location ~scopes e.exp_loc in
      Lprim (
        Pignore,
        [Lprim (Pccall prim, [arg; lbl; newval], loc)],
        loc
      )
  | Texp_setfield(arg, _, lbl, newval) ->
      let access =
        match lbl.lbl_repres with
          Record_regular
        | Record_inlined _ ->
          Psetfield(lbl.lbl_pos, maybe_pointer newval, Assignment)
        | Record_unboxed _ -> assert false
        | Record_float -> Psetfloatfield (lbl.lbl_pos, Assignment)
        | Record_extension _ ->
          Psetfield (lbl.lbl_pos + 1, maybe_pointer newval, Assignment)
      in
      Lprim(access,
            [transl_exp ~ctx ~scopes arg; transl_exp ~ctx ~scopes newval],
            of_location ~scopes e.exp_loc)
  | Texp_array (amut, expr_list) ->
      let kind = array_kind e in
      let ll = transl_list ~ctx ~scopes expr_list in
      let loc = of_location ~scopes e.exp_loc in
      let makearray mutability =
        Lprim (Pmakearray (kind, mutability), ll, loc)
      in
      let duparray_to_mutable array =
        Lprim (Pduparray (kind, Mutable), [array], loc)
      in
      let imm_array = makearray Immutable in
      begin try
        (* For native code the decision as to which compilation strategy to
           use is made later.  This enables the Flambda passes to lift certain
           kinds of array definitions to symbols. *)
        (* Deactivate constant optimization if array is small enough *)
        if amut = Asttypes.Mutable &&
           List.length ll <= use_dup_for_constant_mutable_arrays_bigger_than
        then begin
          raise Not_constant
        end;
        begin match List.map extract_constant ll with
        | exception Not_constant
          when kind = Pfloatarray && amut = Asttypes.Mutable ->
            (* We cannot currently lift mutable [Pintarray] arrays safely in
               Flambda because [caml_modify] might be called upon them
               (e.g. from code operating on polymorphic arrays, or functions
               such as [caml_array_blit].
               To avoid having different Lambda code for bytecode/Closure
               vs. Flambda, we always generate [Pduparray] for mutable arrays
               here, and deal with it in [Bytegen] (or in the case of Closure,
               in [Cmmgen], which already has to handle [Pduparray Pmakearray
               Pfloatarray] in the case where the array turned out to be
               inconstant).
               When not [Pfloatarray], the exception propagates to the handler
               below. *)
            duparray_to_mutable imm_array
        | cl ->
            let const =
              match kind with
              | Paddrarray | Pintarray ->
                  Lconst(Const_block(0, cl))
              | Pfloatarray ->
                  Lconst(Const_float_array(List.map extract_float cl))
              | Pgenarray ->
                  raise Not_constant    (* can this really happen? *)
            in
            match amut with
            | Mutable   -> duparray_to_mutable const
            | Immutable -> const
        end
      with Not_constant ->
        makearray amut
      end
  | Texp_ifthenelse(cond, ifso, Some ifnot) ->
      Lifthenelse(transl_exp ~ctx ~scopes cond,
                  event_before ~scopes ifso (transl_exp ~ctx ~scopes ifso),
                  event_before ~scopes ifnot (transl_exp ~ctx ~scopes ifnot))
  | Texp_ifthenelse(cond, ifso, None) ->
      Lifthenelse(transl_exp ~ctx ~scopes cond,
                  event_before ~scopes ifso (transl_exp ~ctx ~scopes ifso),
                  lambda_unit)
  | Texp_sequence(expr1, expr2) ->
      Lsequence(transl_exp ~ctx ~scopes expr1,
                event_before ~scopes expr2 (transl_exp ~ctx ~scopes expr2))
  | Texp_while(cond, body) ->
      Lwhile(transl_exp ~ctx ~scopes cond,
             event_before ~scopes body (transl_exp ~ctx ~scopes body))
  | Texp_for(param, _, low, high, dir, body) ->
      Lfor(param, transl_exp ~ctx ~scopes low, transl_exp ~ctx ~scopes high,
           dir, event_before ~scopes body (transl_exp ~ctx ~scopes body))
  | Texp_send(expr, met) ->
      let lam =
        let loc = of_location ~scopes e.exp_loc in
        match met with
        | Tmeth_val id ->
            let obj = transl_exp ~ctx ~scopes expr in
            Lsend (Self, Lvar id, obj, [], loc)
        | Tmeth_name nm ->
            let obj = transl_exp ~ctx ~scopes expr in
            let (tag, cache) = Translobj.meth obj nm in
            let kind = if cache = [] then Public else Cached in
            Lsend (kind, tag, obj, cache, loc)
        | Tmeth_ancestor(meth, path_self) ->
            let self = transl_value_path loc e.exp_env path_self in
            Lapply {ap_loc = loc;
                    ap_func = Lvar meth;
                    ap_args = [self];
                    ap_tailcall = Default_tailcall;
                    ap_inlined = Default_inline;
                    ap_specialised = Default_specialise}
      in
      event_after ~scopes e lam
  | Texp_new (cl, {Location.loc=loc}, _) ->
      let loc = of_location ~scopes loc in
      Lapply{
        ap_loc=loc;
        ap_func=
          Lprim(Pfield (0, Pointer, Mutable),
                [transl_class_path loc e.exp_env cl], loc);
        ap_args=[lambda_unit];
        ap_tailcall=Default_tailcall;
        ap_inlined=Default_inline;
        ap_specialised=Default_specialise;
      }
  | Texp_instvar(path_self, path, _) ->
      let loc = of_location ~scopes e.exp_loc in
      let self = transl_value_path loc e.exp_env path_self in
      let var = transl_value_path loc e.exp_env path in
      Lprim(Pfield_computed, [self; var], loc)
  | Texp_setinstvar(path_self, path, _, expr) ->
      let loc = of_location ~scopes e.exp_loc in
      let self = transl_value_path loc e.exp_env path_self in
      let var = transl_value_path loc e.exp_env path in
      transl_setinstvar ~ctx ~scopes loc self var expr
  | Texp_override(path_self, modifs) ->
      let loc = of_location ~scopes e.exp_loc in
      let self = transl_value_path loc e.exp_env path_self in
      let cpy = Ident.create_local "copy" in
      Llet(Strict, Pgenval, cpy,
           Lapply{
             ap_loc=Loc_unknown;
             ap_func=Translobj.oo_prim "copy";
             ap_args=[self];
             ap_tailcall=Default_tailcall;
             ap_inlined=Default_inline;
             ap_specialised=Default_specialise;
           },
           List.fold_right
             (fun (id, _, expr) rem ->
                Lsequence(transl_setinstvar ~ctx ~scopes Loc_unknown
                            (Lvar cpy) (Lvar id) expr, rem))
             modifs
             (Lvar cpy))
  | Texp_pack modl ->
      !transl_module ~ctx ~scopes Tcoerce_none None modl
  | Texp_assert ({exp_desc=Texp_construct(_, {cstr_name="false"}, _)}, loc) ->
      assert_failed loc ~scopes e
  | Texp_assert (cond, loc) ->
      if !Clflags.noassert
      then lambda_unit
      else Lifthenelse (transl_exp ~ctx ~scopes cond, lambda_unit,
                        assert_failed loc ~scopes e)
  | Texp_lazy e ->
      (* when e needs no computation (constants, identifiers, ...), we
         optimize the translation just as Lazy.lazy_from_val would
         do *)
      begin match Typeopt.classify_lazy_argument e with
      | `Constant_or_function ->
        (* A constant expr (of type <> float if [Config.flat_float_array] is
           true) gets compiled as itself. *)
         transl_exp ~ctx ~scopes e
      | `Float_that_cannot_be_shortcut
      | `Identifier `Forward_value ->
         Lprim (Pmakelazyblock Forward_tag,
                [transl_exp ~ctx ~scopes e],
                of_location ~scopes e.exp_loc)
      | `Identifier `Other ->
         transl_exp ~ctx ~scopes e
      | `Other ->
         (* other cases compile to a lazy block holding a function *)
         let fn = lfunction ~kind:Curried
                            ~params:[Ident.create_local "param", Pgenval]
                            ~return:Pgenval
                            (* The translation of [e] may be a function, in
                               which case disallowing arity fusion gives a very
                               small performance improvement.
                            *)
                            ~attr:function_attribute_disallowing_arity_fusion
                            ~loc:(of_location ~scopes e.exp_loc)
                            ~body:(transl_exp ~ctx ~scopes e) in
          Lprim(Pmakelazyblock Lazy_tag, [fn],
                of_location ~scopes e.exp_loc)
      end
  | Texp_object (cs, meths) ->
      let cty = cs.cstr_type in
      let cl = Ident.create_local "object" in
      !transl_object ~ctx ~scopes cl meths
        { cl_desc = Tcl_structure cs;
          cl_loc = e.exp_loc;
          cl_type = Cty_signature cty;
          cl_env = e.exp_env;
          cl_attributes = [];
         }
  | Texp_letop{let_; ands; param; body; partial} ->
      event_after ~scopes e
        (transl_letop ~ctx ~scopes e.exp_loc e.exp_env let_ ands param body
           partial)
  | Texp_quote (e, evidence) ->
    check_no_objects e;
    let body =
      transl_exp ~ctx:{ ctx with quotation_depth = ctx.quotation_depth + 1 }
        ~scopes e
    in
    let body =
      match ctx.macro_env with
      | Some me -> project_captured me body
      | None -> body
    in
    let body =
      match ctx.native_slot_layout with
      | Some nsl -> name_slots_through_block nsl body
      | None -> body
    in
    (* The evidence the quoted code uses, as variable occurrences: the
       quoter makes them free variables of the code value. *)
    let body =
      List.fold_left (fun body ev -> Lsequence (Lvar ev, body)) body evidence
    in
    let body = Translquote.close_quoted ctx.closing body in
    Option.iter
      (fun vars -> vars := Ident.Set.union !vars (free_variables body))
      ctx.quoted_variables;
    Translquote.quote_expression body
  | Texp_splice { spl_top_level = false; spl_exp = e } -> (* Internal splice *)
     let ctx =
       if in_quotation ctx
       then { ctx with quotation_depth = ctx.quotation_depth - 1 }
       else ctx
     in
     let vars = ref Ident.Set.empty in
     let body =
       transl_exp ~ctx:{ ctx with quoted_variables = Some vars } ~scopes e
     in
     Lsplice (close_splice (of_location ~scopes e.exp_loc) !vars body)
  | Texp_splice { spl_exp; spl_top_level = true } -> (* Top-level splice *)
    begin
      match ctx.splice_source with
      | Some hole -> Lsplice (hole ctx spl_exp)
      | None -> Misc.fatal_error "Translcore: splice source is not initialized"
    end
  | Texp_unreachable ->
      raise (Error (e.exp_loc, Unreachable_reached))
  | Texp_struct_item (si, e) ->
      !transl_struct_item ~ctx ~scopes [] None si
        (fun _ -> transl_exp ~ctx ~scopes e)

and pure_module m =
  match m.mod_desc with
    Tmod_ident _ -> Alias
  | Tmod_constraint (m,_,_,_) -> pure_module m
  | _ -> Strict

and transl_list ~ctx ~scopes expr_list =
  List.map (transl_exp ~ctx ~scopes) expr_list

and transl_list_with_shape ~ctx ~scopes expr_list =
  let transl_with_shape e =
    let shape = Typeopt.value_kind e.exp_env e.exp_type in
    transl_exp ~ctx ~scopes e, shape
  in
  List.split (List.map transl_with_shape expr_list)

and transl_guard ~ctx ~scopes guard rhs =
  let expr = event_before ~scopes rhs (transl_exp ~ctx ~scopes rhs) in
  match guard with
  | None -> expr
  | Some cond ->
      event_before ~scopes cond
        (Lifthenelse(transl_exp ~ctx ~scopes cond, expr, staticfail))

and transl_cont cont c_cont body =
  match cont, c_cont with
  | Some id1, Some id2 -> Llet(Alias, Pgenval, id2, Lvar id1, body)
  | None, None
  | Some _, None -> body
  | None, Some _ -> assert false

(* The evidence binder of an arm whose pattern learned equations in
   quoted code: an alias the generated program never reads, which the
   quoter scopes like any binder. *)
and bind_evidence evidence body =
  match evidence with
  | None -> body
  | Some ev ->
      Llet (Alias, Pgenval, ev, Lconst (Const_int 0), body)

and transl_case ~ctx ~scopes ?cont {c_lhs; c_cont; c_guard; c_rhs; c_evidence} =
  (c_lhs,
   transl_cont cont c_cont
     (bind_evidence c_evidence (transl_guard ~ctx ~scopes c_guard c_rhs)))

and transl_cases ~ctx ~scopes ?cont cases =
  let cases =
    List.filter (fun c -> c.c_rhs.exp_desc <> Texp_unreachable) cases in
  List.map (transl_case ~ctx ~scopes ?cont) cases

and transl_case_try ~ctx ~scopes {c_lhs; c_guard; c_rhs; c_evidence} =
  iter_exn_names Translprim.add_exception_ident c_lhs;
  Misc.try_finally
    (fun () ->
       c_lhs,
       bind_evidence c_evidence (transl_guard ~ctx ~scopes c_guard c_rhs))
    ~always:(fun () ->
        iter_exn_names Translprim.remove_exception_ident c_lhs)

and transl_cases_try ~ctx ~scopes cases =
  let cases =
    List.filter (fun c -> c.c_rhs.exp_desc <> Texp_unreachable) cases in
  List.map (transl_case_try ~ctx ~scopes) cases

and transl_tupled_cases ~ctx ~scopes patl_expr_list =
  let patl_expr_list =
    List.filter (fun (_,_,e,_) -> e.exp_desc <> Texp_unreachable)
      patl_expr_list in
  List.map
    (fun (patl, guard, expr, evidence) ->
       (patl, bind_evidence evidence (transl_guard ~ctx ~scopes guard expr)))
    patl_expr_list

and transl_apply ~ctx ~scopes
      ?(tailcall=Default_tailcall)
      ?(inlined = Default_inline)
      ?(specialised = Default_specialise)
      lam sargs loc
  =
  let lapply funct args =
    match funct with
      Lsend(k, lmet, lobj, largs, _) ->
        Lsend(k, lmet, lobj, largs @ args, loc)
    | Levent(Lsend(k, lmet, lobj, largs, _), _) ->
        Lsend(k, lmet, lobj, largs @ args, loc)
    | Lapply ap ->
        Lapply {ap with ap_args = ap.ap_args @ args; ap_loc = loc}
    | lexp ->
        Lapply {
          ap_loc=loc;
          ap_func=lexp;
          ap_args=args;
          ap_tailcall=tailcall;
          ap_inlined=inlined;
          ap_specialised=specialised;
        }
  in
  (* Build a function application.
     Particular care is required for out-of-order partial applications.
     The following code guarantees that:
     * arguments are evaluated right-to-left according to their order in
       the type of the function, before the function is called;
     * side-effects occurring after receiving a non-optional parameter
       will occur exactly when all the arguments up to this parameter
       have been received;
     * side-effects occurring after receiving an optional parameter
       will occur at the latest when all the arguments up to the first
       non-optional parameter that follows it have been received.
  *)
  let rec build_apply lam args = function
      (Omitted (), optional) :: l ->
        (* Out-of-order partial application; we will need to build a closure *)
        let defs = ref [] in
        let protect name lam =
          match lam with
            Lvar _ | Lconst _ -> lam
          | _ ->
              let id = Ident.create_local name in
              defs := (id, lam) :: !defs;
              Lvar id
        in
        (* If all arguments in [args] were optional, delay their application
           until after this one is received *)
        let args, args' =
          if List.for_all (fun (_,opt) -> opt) args then [], args
          else args, []
        in
        let lam =
          if args = [] then lam else lapply lam (List.rev_map fst args)
        in
        (* Evaluate the function, applied to the arguments in [args] *)
        let handle = protect "func" lam in
        (* Evaluate the arguments whose applications was delayed;
           if we already passed here this is a no-op. *)
        let args' =
          List.map (fun (arg, opt) -> protect "arg" arg, opt) args'
        in
        (* Evaluate the remaining arguments;
           if we already passed here this is a no-op. *)
        let l =
          List.map
            (fun (arg, opt) -> Typedtree.map_apply_arg (protect "arg") arg, opt)
            l
        in
        let id_arg = Ident.create_local "param" in
        (* Process remaining arguments and build closure *)
        let body =
          match build_apply handle ((Lvar id_arg, optional)::args') l with
            Lfunction{kind = Curried; params = ids; return; body; attr; loc}
            when List.length ids < Lambda.max_arity () ->
              lfunction ~kind:Curried ~params:((id_arg, Pgenval)::ids)
                        ~return ~body ~attr ~loc
          | body ->
              lfunction ~kind:Curried ~params:[id_arg, Pgenval]
                        ~return:Pgenval ~body
                        ~attr:default_stub_attribute ~loc
        in
        (* Wrap "protected" definitions, starting from the left,
           so that evaluation is right-to-left. *)
        List.fold_right
          (fun (id, lam) body -> Llet(Strict, Pgenval, id, lam, body))
          !defs body
    | (Arg arg, optional) :: l ->
        build_apply lam ((arg, optional) :: args) l
    | [] ->
        lapply lam (List.rev_map fst args)
  in
  let transl_arg arg = Typedtree.map_apply_arg (transl_exp ~ctx ~scopes) arg in
  (build_apply lam [] (List.map (fun (l, arg) ->
                                   transl_arg arg,
                                   Btype.is_optional l)
                                sargs)
     : Lambda.lambda)

(* There are two cases in function translation:
    - [Tupled]. It takes a tupled argument, and we can flatten it.
    - [Curried]. It takes each argument individually.

   We first try treating the function as taking a flattened tupled argument (in
   [trans_tupled_function]) and, if that doesn't work, we fall back to treating
   the function as taking each argument individually (in
   [trans_curried_function]).
*)
and transl_function_without_attributes ~ctx ~scopes loc repr params body =
  let return =
    match body with
    | Tfunction_body body ->
        value_kind body.exp_env body.exp_type
    | Tfunction_cases { cases = { c_rhs } :: _ } ->
        value_kind c_rhs.exp_env c_rhs.exp_type
    | Tfunction_cases { cases = [] } ->
        (* With Camlp4/ppx, a pattern matching might be empty *)
        Pgenval
  in
  transl_tupled_function ~ctx ~scopes loc return repr params body

and transl_tupled_function ~ctx ~scopes loc return repr params body =
  (* Cases are eligible for flattening if they belong to the only param. *)
  let eligible_cases =
    match params, body with
    | [], Tfunction_cases { cases; partial } ->
        Some (cases, partial)
    | [ { fp_kind = Tparam_pat pat; fp_partial; fp_evidence } ],
      Tfunction_body body ->
        let case =
          { c_lhs = pat; c_cont = None; c_guard = None; c_rhs = body;
            c_evidence = fp_evidence }
        in
        Some ([ case ], fp_partial)
    | _ -> None
  in
  match eligible_cases with
  | Some (({ c_lhs = { pat_desc = Tpat_tuple pl } } :: _) as cases, partial)
    when !Clflags.native_code
      && List.length pl <= (Lambda.max_arity ()) ->
      begin try
        let size = List.length pl in
        let pats_expr_list =
          List.map
            (fun {c_lhs; c_guard; c_rhs; c_evidence} ->
              (Matching.flatten_pattern size c_lhs, c_guard, c_rhs,
               c_evidence))
            cases in
        let kinds =
          (* All the patterns might not share the same types. We must take the
             union of the patterns types *)
          match pats_expr_list with
          | [] -> assert false
          | (pats, _, _, _) :: cases ->
              let first_case_kinds =
                List.map (fun pat -> value_kind pat.pat_env pat.pat_type) pats
              in
              List.fold_left
                (fun kinds (pats, _, _, _) ->
                  List.map2 (fun kind pat ->
                    value_kind_union kind
                      (value_kind pat.pat_env pat.pat_type))
                    kinds pats)
                first_case_kinds cases
        in
        let tparams =
          List.map (fun kind -> Ident.create_local "param", kind) kinds
        in
        let params = List.map fst tparams in
        ((Tupled, tparams, return),
         Matching.for_tupled_function ~scopes loc params
           (transl_tupled_cases ~ctx ~scopes pats_expr_list) partial)
    with Matching.Cannot_flatten ->
      transl_curried_function ~ctx ~scopes loc return repr params body
      end
  | _ -> transl_curried_function ~ctx ~scopes loc return repr params body

and transl_curried_function ~ctx ~scopes loc return repr params body =
  let cases_param, body =
    match body with
    | Tfunction_body body ->
        None, event_before ~scopes body (transl_exp ~ctx ~scopes body)
    | Tfunction_cases { cases; partial; param; loc = cases_loc } ->
        let kind =
          match cases with
          | [] ->
            (* With Camlp4/ppx, a pattern matching might be empty *)
            Pgenval
          | {c_lhs=pat} :: other_cases ->
            (* All the patterns might not share the same types. We must take the
              union of the patterns types *)
            List.fold_left (fun k {c_lhs=pat} ->
              Typeopt.value_kind_union k
                (value_kind pat.pat_env pat.pat_type))
              (value_kind pat.pat_env pat.pat_type) other_cases
        in
        let body =
          Matching.for_function ~scopes cases_loc repr (Lvar param)
            (transl_cases ~ctx ~scopes cases) partial
        in
        Some (param, kind), body
  in
  let body, params =
    List.fold_right (fun fp (body, params) ->
      let param = fp.fp_param in
      let param_loc = fp.fp_loc in
      match fp.fp_kind with
      | Tparam_pat pat ->
          let kind = value_kind pat.pat_env pat.pat_type in
          let body =
            Matching.for_function ~scopes param_loc None (Lvar param)
              [ pat, bind_evidence fp.fp_evidence body ]
              fp.fp_partial
          in
          body, (param, kind) :: params
      | Tparam_optional_default (pat, default_arg) ->
          let default_arg =
            event_before ~scopes default_arg
              (transl_exp ~ctx ~scopes default_arg)
          in
          let body =
            Matching.for_optional_arg_default
              ~scopes param_loc pat body ~default_arg ~param
          in
          (* The optional param is Pgenval as it's an option. *)
          body, (param, Pgenval) :: params)
    params
    (body, Option.to_list cases_param)
  in
  (* chunk params according to Lambda.max_arity. If Lambda.max_arity = n and
     N>n, then the translation of an N-ary typedtree function is an n-ary lambda
     function returning the translation of an (N-n)-ary typedtree function.
  *)
  let params, return, body =
    match Misc.Stdlib.List.chunks_of (Lambda.max_arity ()) params with
    | [] ->
        Misc.fatal_error "attempted to translate a function with zero arguments"
    | first_chunk :: rest_of_chunks ->
      let body, return =
        List.fold_right
          (fun chunk (body, return) ->
            let attr = function_attribute_disallowing_arity_fusion in
            let loc = of_location ~scopes loc in
            let body =
              lfunction ~kind:Curried ~params:chunk ~return ~body ~attr ~loc
            in
            (* we return Pgenval (for a function) after the rightmost chunk. *)
            body, Pgenval)
          rest_of_chunks
          (body, return)
      in
      first_chunk, return, body
  in
  ((Curried, params, return), body)

and transl_function ~ctx ~scopes e params body =
  let ((kind, params, return), body) =
    event_function ~scopes e
      (function repr ->
         let params, body = fuse_method_arity params body in
         transl_function_without_attributes ~ctx ~scopes e.exp_loc repr params
           body)
  in
  let attr = function_attribute_disallowing_arity_fusion in
  let loc = of_location ~scopes e.exp_loc in
  let lam = lfunction ~kind ~params ~return ~body ~attr ~loc in
  let attrs =
    (* Collect attributes from the Pexp_newtype node for locally abstract types.
       Otherwise we'd ignore the attribute in, e.g.:
           fun [@inline] (type a) x -> ...
    *)
    List.fold_left
      (fun attrs (extra_exp, _, extra_attrs) ->
         match extra_exp with
         | Texp_newtype _ -> extra_attrs @ attrs
         | (Texp_constraint _ | Texp_coerce _ | Texp_poly _) -> attrs)
      e.exp_attributes e.exp_extra
  in
  Translattribute.add_function_attributes lam e.exp_loc attrs

(* Like transl_exp, but used when a new scope was just introduced. *)
and transl_scoped_exp ~ctx ~scopes expr =
  transl_exp1 ~ctx ~scopes ~in_new_scope:true expr

(* Decides whether a pattern binding should introduce a new scope. *)
and transl_bound_exp ~ctx ~scopes ~in_structure pat expr =
  let should_introduce_scope =
    match expr.exp_desc with
    | Texp_function _ -> true
    | _ when in_structure -> true
    | _ -> false in
  match pat_bound_idents pat with
  | (id :: _) when should_introduce_scope ->
     transl_scoped_exp ~ctx ~scopes:(enter_value_definition ~scopes id) expr
  | _ -> transl_exp ~ctx ~scopes expr

(*
  Notice: transl_let consumes (ie compiles) its pat_expr_list argument,
  and returns a function that will take the body of the lambda-let construct.
  This complication allows choosing any compilation order for the
  bindings and body of let constructs.
*)
and transl_let ~ctx ~scopes ?(in_structure=false) rec_flag pat_expr_list =
  match rec_flag with
    Nonrecursive ->
      let rec transl = function
        [] ->
          fun body -> body
      | {vb_pat=pat; vb_expr=expr; vb_rec_kind=_; vb_attributes=attr; vb_loc}
        :: rem ->
          let lam = transl_bound_exp ~ctx ~scopes ~in_structure pat expr in
          let lam = Translattribute.add_function_attributes lam vb_loc attr in
          let mk_body = transl rem in
          fun body ->
            Matching.for_let ~scopes pat.pat_loc lam pat (mk_body body)
      in transl pat_expr_list
  | Recursive ->
      let idlist =
        List.map
          (fun {vb_pat=pat} -> match pat.pat_desc with
              Tpat_var (id,_,_) -> id
            | _ -> assert false)
        pat_expr_list in
      let transl_case {vb_expr=expr; vb_attributes; vb_rec_kind = rkind;
                       vb_loc; vb_pat} id =
        let def = transl_bound_exp ~ctx ~scopes ~in_structure vb_pat expr in
        let def =
          Translattribute.add_function_attributes def vb_loc vb_attributes
        in
        ( id, rkind, def ) in
      let lam_bds = List.map2 transl_case pat_expr_list idlist in
      fun body -> Value_rec_compiler.compile_letrec lam_bds body

and transl_setinstvar ~ctx ~scopes loc self var expr =
  Lprim(Psetfield_computed (maybe_pointer expr, Assignment),
    [self; var; transl_exp ~ctx ~scopes expr], loc)

and transl_record ~ctx ~scopes loc env fields repres opt_init_expr =
  let size = Array.length fields in
  (* Determine if there are "enough" fields (only relevant if this is a
     functional-style record update *)
  let no_init = match opt_init_expr with None -> true | _ -> false in
  if no_init || size < Config.max_young_wosize
  then begin
    (* Allocate new record with given fields (and remaining fields
       taken from init_expr if any *)
    let init_id = Ident.create_local "init" in
    let lv =
      Array.mapi
        (fun i (_, definition) ->
           match definition with
           | Kept (typ, mut) ->
               let field_kind = value_kind env typ in
               let access =
                 match repres with
                   Record_regular | Record_inlined _ ->
                     Pfield (i, maybe_pointer_type env typ, mut)
                 | Record_unboxed _ -> assert false
                 | Record_extension _ ->
                     Pfield (i + 1, maybe_pointer_type env typ, mut)
                 | Record_float -> Pfloatfield i in
               Lprim(access, [Lvar init_id],
                     of_location ~scopes loc),
               field_kind
           | Overridden (_lid, expr) ->
               let field_kind = value_kind expr.exp_env expr.exp_type in
               transl_exp ~ctx ~scopes expr, field_kind)
        fields
    in
    let ll, shape = List.split (Array.to_list lv) in
    let mut =
      if Array.exists (fun (lbl, _) -> lbl.lbl_mut = Mutable) fields
      then Mutable
      else Immutable in
    let lam =
      try
        if mut = Mutable then raise Not_constant;
        let cl = List.map extract_constant ll in
        match repres with
        | Record_regular -> Lconst(Const_block(0, cl))
        | Record_inlined tag -> Lconst(Const_block(tag, cl))
        | Record_unboxed _ -> Lconst(match cl with [v] -> v | _ -> assert false)
        | Record_float ->
            Lconst(Const_float_array(List.map extract_float cl))
        | Record_extension _ ->
            raise Not_constant
      with Not_constant ->
        let loc = of_location ~scopes loc in
        match repres with
          Record_regular ->
            Lprim(Pmakeblock(0, mut, Some shape), ll, loc)
        | Record_inlined tag ->
            Lprim(Pmakeblock(tag, mut, Some shape), ll, loc)
        | Record_unboxed _ -> (match ll with [v] -> v | _ -> assert false)
        | Record_float ->
            Lprim(Pmakearray (Pfloatarray, mut), ll, loc)
        | Record_extension path ->
            let slot = transl_extension_path loc env path in
            Lprim(Pmakeblock(0, mut, Some (Pgenval :: shape)), slot :: ll, loc)
    in
    begin match opt_init_expr with
      None -> lam
    | Some init_expr -> Llet(Strict, Pgenval, init_id,
                             transl_exp ~ctx ~scopes init_expr, lam)
    end
  end else begin
    (* Take a shallow copy of the init record, then mutate the fields
       of the copy *)
    let copy_id = Ident.create_local "newrecord" in
    let update_field cont (lbl, definition) =
      match definition with
      | Kept _ -> cont
      | Overridden (_lid, expr) ->
          let upd =
            match repres with
              Record_regular
            | Record_inlined _ ->
                Psetfield(lbl.lbl_pos, maybe_pointer expr, Assignment)
            | Record_unboxed _ -> assert false
            | Record_float -> Psetfloatfield (lbl.lbl_pos, Assignment)
            | Record_extension _ ->
                Psetfield(lbl.lbl_pos + 1, maybe_pointer expr, Assignment)
          in
          Lsequence(Lprim(upd, [Lvar copy_id; transl_exp ~ctx ~scopes expr],
                          of_location ~scopes loc),
                    cont)
    in
    begin match opt_init_expr with
      None -> assert false
    | Some init_expr ->
        Llet(Strict, Pgenval, copy_id,
             Lprim(Pduprecord (repres, size),
                   [transl_exp ~ctx ~scopes init_expr],
                   of_location ~scopes loc),
             Array.fold_left update_field (Lvar copy_id) fields)
    end
  end

and transl_atomic_loc ~ctx ~scopes arg lbl =
  let arg = transl_exp ~ctx ~scopes arg in
  let offset =
    match lbl.lbl_repres with
    | Record_regular
    | Record_inlined _ -> 0
    | Record_float ->
        fatal_error
          "Translcore.transl_atomic_loc: atomic field in float record"
    | Record_unboxed _ ->
        fatal_error
          "Translcore.transl_atomic_loc: atomic field in unboxed record"
    | Record_extension _ -> 1
  in
  let lbl = Lconst (Const_int (lbl.lbl_pos + offset)) in
  (arg, lbl)

and transl_match ~ctx ~scopes e arg pat_expr_list partial =
  let rewrite_case (val_cases, exn_cases, static_handlers as acc)
        ({ c_lhs; c_guard; c_rhs } as case) =
    if c_rhs.exp_desc = Texp_unreachable then acc else
    let val_pat, exn_pat = split_pattern c_lhs in
    match val_pat, exn_pat with
    | None, None -> assert false
    | Some pv, None ->
        let val_case =
          transl_case ~ctx ~scopes { case with c_lhs = pv }
        in
        val_case :: val_cases, exn_cases, static_handlers
    | None, Some pe ->
        let exn_case = transl_case_try ~ctx ~scopes { case with c_lhs = pe } in
        val_cases, exn_case :: exn_cases, static_handlers
    | Some pv, Some pe ->
        assert (c_guard = None);
        let lbl  = next_raise_count () in
        let static_raise ids =
          Lstaticraise (lbl, List.map (fun id -> Lvar id) ids)
        in
        (* Simplif doesn't like it if binders are not uniq, so we make sure to
           use different names in the value and the exception branches. *)
        let ids_full = Typedtree.pat_bound_idents_full pv in
        let ids = List.map (fun (id, _, _, _) -> id) ids_full in
        let ids_kinds =
          List.map (fun (id, _, ty, _) -> id, Typeopt.value_kind pv.pat_env ty)
            ids_full
        in
        let vids = List.map Ident.rename ids in
        let pv = alpha_pat (List.combine ids vids) pv in
        (* Also register the names of the exception so Re-raise happens. *)
        iter_exn_names Translprim.add_exception_ident pe;
        let rhs =
          Misc.try_finally
            (fun () -> event_before ~scopes c_rhs
                         (transl_exp ~ctx ~scopes c_rhs))
            ~always:(fun () ->
                iter_exn_names Translprim.remove_exception_ident pe)
        in
        (pv, static_raise vids) :: val_cases,
        (pe, static_raise ids) :: exn_cases,
        (lbl, ids_kinds, rhs) :: static_handlers
  in
  let val_cases, exn_cases, static_handlers =
    let x, y, z = List.fold_left rewrite_case ([], [], []) pat_expr_list in
    List.rev x, List.rev y, List.rev z
  in
  (* In presence of exception patterns, the code we generate for

       match <scrutinees> with
       | <val-patterns> -> <val-actions>
       | <exn-patterns> -> <exn-actions>

     looks like

       staticcatch
         (try (exit <val-exit> <scrutinees>)
          with <exn-patterns> -> <exn-actions>)
       with <val-exit> <val-ids> ->
          match <val-ids> with <val-patterns> -> <val-actions>

     In particular, the 'exit' in the value case ensures that the
     value actions run outside the try..with exception handler.
  *)
  let static_catch scrutinees val_ids handler =
    let id = Typecore.name_pattern "exn" (List.map fst exn_cases) in
    let static_exception_id = next_raise_count () in
    Lstaticcatch
      (Ltrywith (Lstaticraise (static_exception_id, scrutinees), id,
                 Matching.for_trywith ~scopes e.exp_loc (Lvar id) exn_cases),
       (static_exception_id, val_ids),
       handler)
  in
  let classic =
    match arg, exn_cases with
    | {exp_desc = Texp_tuple argl}, [] ->
      assert (static_handlers = []);
      Matching.for_multiple_match ~scopes e.exp_loc
        (transl_list ~ctx ~scopes (List.map snd argl)) val_cases partial
    | {exp_desc = Texp_tuple argl}, _ :: _ ->
        let argl = List.map snd argl in
        let val_ids =
          List.map
            (fun arg ->
               Typecore.name_pattern "val" [],
               Typeopt.value_kind arg.exp_env arg.exp_type
            )
            argl
        in
        let lvars = List.map (fun (id, _) -> Lvar id) val_ids in
        static_catch (transl_list ~ctx ~scopes argl) val_ids
          (Matching.for_multiple_match ~scopes e.exp_loc
             lvars val_cases partial)
    | arg, [] ->
      assert (static_handlers = []);
      Matching.for_function ~scopes e.exp_loc
        None (transl_exp ~ctx ~scopes arg) val_cases partial
    | arg, _ :: _ ->
        let val_id = Typecore.name_pattern "val" (List.map fst val_cases) in
        let k = Typeopt.value_kind arg.exp_env arg.exp_type in
        static_catch [transl_exp ~ctx ~scopes arg] [val_id, k]
          (Matching.for_function ~scopes e.exp_loc
             None (Lvar val_id) val_cases partial)
  in
  List.fold_left (fun body (static_exception_id, val_ids, handler) ->
    Lstaticcatch (body, (static_exception_id, val_ids), handler)
  ) classic static_handlers

and prim_alloc_stack =
  Pccall (Primitive.simple ~name:"caml_alloc_stack" ~arity:3 ~alloc:true)

and transl_handler ~ctx ~scopes e body val_caselist exn_caselist eff_caselist =
  let val_fun =
    match val_caselist with
    | None ->
        let param = Ident.create_local "param" in
        lfunction ~kind:Curried ~params:[param, Pgenval]
         ~return:Pgenval ~body:(Lvar param)
         ~attr:default_function_attribute ~loc:Loc_unknown
    | Some (val_caselist, partial) ->
        let val_cases = transl_cases ~ctx ~scopes val_caselist in
        let param = Typecore.name_cases "param" val_caselist in
        let body =
          Matching.for_function ~scopes e.exp_loc None (Lvar param) val_cases
            partial
        in
        lfunction ~kind:Curried ~params:[param, Pgenval]
          ~return:Pgenval ~attr:default_function_attribute
          ~loc:Loc_unknown ~body
  in
  let exn_fun =
    let exn_cases = transl_cases ~ctx ~scopes exn_caselist in
    let param = Typecore.name_cases "exn" exn_caselist in
    let body = Matching.for_trywith ~scopes e.exp_loc (Lvar param) exn_cases in
    lfunction ~kind:Curried ~params:[param, Pgenval] ~return:Pgenval
      ~attr:default_function_attribute ~loc:Loc_unknown ~body
  in
  let eff_fun =
    let param = Typecore.name_cases "eff" eff_caselist in
    let cont = Ident.create_local "k" in
    let cont_tail = Ident.create_local "ktail" in
    let eff_cases = transl_cases ~ctx ~scopes ~cont eff_caselist in
    let body =
      Matching.for_handler ~scopes e.exp_loc (Lvar param) (Lvar cont)
        (Lvar cont_tail) eff_cases
    in
    lfunction ~kind:Curried
      ~params:[(param, Pgenval); (cont, Pgenval); (cont_tail, Pgenval)]
      ~return:Pgenval ~attr:default_function_attribute ~loc:Loc_unknown ~body
  in
  let (body_fun, arg) =
    match transl_exp ~ctx ~scopes body with
    | Lapply { ap_func = fn; ap_args = [arg]; _ }
        when is_evaluated fn && is_evaluated arg -> (fn, arg)
    | body ->
       let param = Ident.create_local "param" in
       (lfunction ~kind:Curried ~params:[param, Pgenval] ~return:Pgenval
                  ~attr:default_function_attribute ~loc:Loc_unknown
                  ~body,
        Lconst(Const_int 0))
  in
  let alloc_stack =
    Lprim(prim_alloc_stack, [val_fun; exn_fun; eff_fun], Loc_unknown)
  in
  Lprim(Prunstack, [alloc_stack; body_fun; arg],
        of_location ~scopes e.exp_loc)

and transl_letop ~ctx ~scopes loc env let_ ands param case partial =
  let rec loop prev_lam = function
    | [] -> prev_lam
    | and_ :: rest ->
        let left_id = Ident.create_local "left" in
        let right_id = Ident.create_local "right" in
        let op =
          transl_ident ~ctx (of_location ~scopes and_.bop_op_name.loc) env
            and_.bop_op_type and_.bop_op_path and_.bop_op_val
        in
        let exp = transl_exp ~ctx ~scopes and_.bop_exp in
        let lam =
          bind Strict right_id exp
            (Lapply{
               ap_loc = of_location ~scopes and_.bop_loc;
               ap_func = op;
               ap_args=[Lvar left_id; Lvar right_id];
               ap_tailcall = Default_tailcall;
               ap_inlined = Default_inline;
               ap_specialised = Default_specialise;
             })
        in
        bind Strict left_id prev_lam (loop lam rest)
  in
  let op =
    transl_ident ~ctx (of_location ~scopes let_.bop_op_name.loc) env
      let_.bop_op_type let_.bop_op_path let_.bop_op_val
  in
  let exp = loop (transl_exp ~ctx ~scopes let_.bop_exp) ands in
  let func =
    let (kind, params, return), body =
      event_function ~scopes case.c_rhs
        (function repr ->
           let loc = case.c_rhs.exp_loc in
           let ghost_loc = { loc with loc_ghost = true } in
           transl_function_without_attributes ~ctx ~scopes loc repr []
             (Tfunction_cases
                { cases = [case]; param; partial; loc = ghost_loc;
                  exp_extra = None; attributes = []; }))
    in
    let attr = function_attribute_disallowing_arity_fusion in
    let loc = of_location ~scopes case.c_rhs.exp_loc in
    lfunction ~kind ~params ~return ~body ~attr ~loc
  in
  Lapply{
    ap_loc = of_location ~scopes loc;
    ap_func = op;
    ap_args=[exp; func];
    ap_tailcall = Default_tailcall;
    ap_inlined = Default_inline;
    ap_specialised = Default_specialise;
  }

(* The variables of run-time code that the quotations of [e], which is
   compile-time code, mention: those that the code it builds is closed
   over.  [e] is translated to find them, and the translation is
   dropped.  A variable of another unit is reached through its global,
   and is not one of them. *)
let quoted_variables ~ctx ~scopes e =
  let vars = ref Ident.Set.empty in
  let ctx =
    { ctx with quoted_variables = Some vars; closing = Ident.Map.empty;
               macro_env = None }
  in
  ignore (transl_exp ~ctx ~scopes e);
  Ident.Set.filter (fun id -> not (Ident.global id)) !vars

(* Wrapper for class compilation *)

(*
let transl_exp = transl_exp_wrap

let transl_let rec_flag pat_expr_list body =
  match pat_expr_list with
    [] -> body
  | (_, expr) :: _ ->
      Translobj.oo_wrap expr.exp_env false
        (transl_let rec_flag pat_expr_list) body
*)

(* Error report *)

open Format_doc

let report_error_doc ppf = function
  | Free_super_var ->
      fprintf ppf
        "Ancestor names can only be used to select inherited methods"
  | Unreachable_reached ->
      fprintf ppf "Unreachable expression was reached"
  | Objects_in_staged_code ->
      fprintf ppf
        "Objects and classes are not yet supported in quotations,@ \
         or in compile-time code."

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
          Some (Location.error_of_printer ~loc report_error_doc err)
      | _ ->
        None
    )

let report_error = Format_doc.compat report_error_doc
