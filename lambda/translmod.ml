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
   for the module language *)

open Misc
open Asttypes
open Path
open Types
open Typedtree
open Lambda
open Translobj
open Translcore
open Translclass
open Debuginfo.Scoped_location

type unsafe_component =
  | Unsafe_module_binding
  | Unsafe_functor
  | Unsafe_non_function
  | Unsafe_typext

type unsafe_info =
  | Unsafe of {
      reason:unsafe_component;
      loc:Location.t;
      path: Path.t
    }
  | Unnamed

type error =
  Circular_dependency of (Ident.t * unsafe_info) list
| Conflicting_inline_attributes
| Static_quotes_local of Ident.t

exception Error of Location.t * error

(* The translation of a module expression or of an item builds the part
   of the unit that [ctx.world] gives. *)
let in_compile_world ctx = ctx.world = Compile_world
let in_run_world ctx = not (in_compile_world ctx)

let in_world ctx world f = f { ctx with world }

let record_compile_world_dummy ctx id =
  let st = ctx.unit_state in
  st.compile_world_dummies <- Ident.Set.add id st.compile_world_dummies

(* Only the compile-time part asks: the run-time part binds the same
   identifiers to their values. *)
let is_compile_world_dummy ctx = function
  | Lvar id ->
      in_compile_world ctx
      && Ident.Set.mem id ctx.unit_state.compile_world_dummies
  | Lconst c -> in_compile_world ctx && c = const_unit
  | _ -> false

let module_address_is_dummy ctx env path =
  let rec root = function
    | Env.Aident id -> Ident.Set.mem id ctx.unit_state.compile_world_dummies
    | Env.Adot (a, _) -> root a
  in
  match Env.find_module_address path env with
  | addr -> root addr
  | exception Not_found -> false

(* In the compile-time part a module that has compile-time components
   is its compile-time block, [M$macros] for a unit [M]: the run-time
   objects are not linked at compile time.  This definition shadows
   that of [Lambda]. *)
let transl_module_path ~ctx loc env path =
  if not (in_compile_world ctx) then Lambda.transl_module_path loc env path
  else if
    match Env.find_module path env with
    | md -> Mtype.has_macro_components env md.md_type
    | exception Not_found -> false
  then transl_module_macros_path loc env path
  else if module_address_is_dummy ctx env path then Lconst const_unit
  else Lambda.transl_module_path loc env path

let cons_opt x_opt xs =
  match x_opt with
  | None -> xs
  | Some x -> x :: xs

(* Keep track of the root path (from the root of the namespace to the
   currently compiled module expression).  Useful for naming extensions. *)

let global_path glob = Some(Pident glob)
let functor_path path param =
  match path with
    None -> None
  | Some p -> Some(Papply(p, Pident param))
let field_path path field =
  match path with
    None -> None
  | Some p -> Some(Pdot(p, Ident.name field))

(* Compile type extensions *)

(* An extension constructor is allocated in the run-time part. *)
let transl_extension ~ctx ~scopes env rootpath ext =
  if in_run_world ctx then
    transl_extension_constructor ~scopes env
      (field_path rootpath ext.ext_id) ext
  else begin
    record_compile_world_dummy ctx ext.ext_id;
    Lconst const_unit
  end

let transl_type_extension ~ctx ~scopes env rootpath tyext body =
  List.fold_right
    (fun ext body ->
      let lam = transl_extension ~ctx ~scopes env rootpath ext in
      Llet(Strict, Pgenval, ext.ext_id, lam, body))
    tyext.tyext_constructors
    body

(* Compile a coercion *)

let rec apply_coercion ~ctx loc strict restr arg =
  match restr with
    Tcoerce_none ->
      arg
  | (Tcoerce_structure _ | Tcoerce_functor _)
    when is_compile_world_dummy ctx arg ->
      arg
  | Tcoerce_structure(pos_cc_list, id_pos_list) ->
      name_lambda strict arg (fun id ->
        let get_field pos =
          if pos < 0 then lambda_unit
          else Lprim(Pfield (pos, Pointer, Mutable), [Lvar id], loc)
        in
        let lam =
          Lprim(Pmakeblock(0, Immutable, None),
                List.map (apply_coercion_field ~ctx loc get_field) pos_cc_list,
                loc)
        in
        wrap_id_pos_list ~ctx loc id_pos_list get_field lam)
  | Tcoerce_functor(cc_arg, cc_res) ->
      let param = Ident.create_local "funarg" in
      let carg = apply_coercion ~ctx loc Alias cc_arg (Lvar param) in
      apply_coercion_result ~ctx loc strict arg [param, Pgenval] [carg] cc_res
  | Tcoerce_primitive { pc_loc = _; pc_desc; pc_env; pc_type; } ->
      Translprim.transl_primitive loc pc_desc pc_env pc_type None
  | Tcoerce_alias (env, path, cc) ->
      let lam = transl_module_path ~ctx loc env path in
      name_lambda strict arg
        (fun _ -> apply_coercion ~ctx loc Alias cc lam)

and apply_coercion_field ~ctx loc get_field (pos, cc) =
  apply_coercion ~ctx loc Alias cc (get_field pos)

and apply_coercion_result ~ctx loc strict funct params args cc_res =
  match cc_res with
  | Tcoerce_functor(cc_arg, cc_res) ->
    let param = Ident.create_local "funarg" in
    let arg = apply_coercion ~ctx loc Alias cc_arg (Lvar param) in
    apply_coercion_result ~ctx loc strict funct
      ((param, Pgenval) :: params) (arg :: args) cc_res
  | _ ->
      name_lambda strict funct
        (fun id ->
           lfunction
             ~kind:Curried
             ~params:(List.rev params)
             ~return:Pgenval
             ~attr:{ default_function_attribute with
                        is_a_functor = true;
                        stub = true;
                        may_fuse_arity = true; }
             ~loc
             ~body:(apply_coercion ~ctx
                   loc Strict cc_res
                   (Lapply{
                      ap_loc=loc;
                      ap_func=Lvar id;
                      ap_args=List.rev args;
                      ap_tailcall=Default_tailcall;
                      ap_inlined=Default_inline;
                      ap_specialised=Default_specialise;
                    })))

and wrap_id_pos_list ~ctx loc id_pos_list get_field lam =
  let fv = free_variables lam in
  (*Format.eprintf "%a@." Printlambda.lambda lam;
  Ident.Set.iter (fun id -> Format.eprintf "%a " Ident.print id) fv;
  Format.eprintf "@.";*)
  let (lam, _fv, s) =
    List.fold_left (fun (lam, fv, s) (id',pos,c) ->
      if Ident.Set.mem id' fv then
        let id'' = Ident.create_local (Ident.name id') in
        let rhs = apply_coercion ~ctx loc Alias c (get_field pos) in
        let fv_rhs = free_variables rhs in
        (Llet(Alias, Pgenval, id'', rhs, lam),
         Ident.Set.union fv fv_rhs,
         Ident.Map.add id' id'' s)
      else (lam, fv, s))
      (lam, fv, Ident.Map.empty) id_pos_list
  in
  if s == Ident.Map.empty then lam else Lambda.rename s lam


(* Compose two coercions
   apply_coercion ~ctx c1 (apply_coercion ~ctx c2 e) behaves like
   apply_coercion ~ctx (compose_coercions c1 c2) e. *)

let rec compose_coercions c1 c2 =
  match (c1, c2) with
    (Tcoerce_none, c2) -> c2
  | (c1, Tcoerce_none) -> c1
  | (Tcoerce_structure (pc1, ids1), Tcoerce_structure (pc2, ids2)) ->
      let v2 = Array.of_list pc2 in
      let ids1 =
        List.map (fun (id,pos1,c1) ->
            if pos1 < 0 then (id, pos1, c1)
            else
              let (pos2,c2) = v2.(pos1) in
              (id, pos2, compose_coercions c1 c2))
          ids1
      in
      Tcoerce_structure
        (List.map
           (fun pc ->
              match pc with
              | _, (Tcoerce_primitive _ | Tcoerce_alias _) ->
                (* These cases do not take an argument (the position is -1),
                   so they do not need adjusting. *)
                pc
              | (p1, c1) ->
                let (p2, c2) = v2.(p1) in
                (p2, compose_coercions c1 c2))
          pc1,
         ids1 @ ids2)
  | (Tcoerce_functor(arg1, res1), Tcoerce_functor(arg2, res2)) ->
      Tcoerce_functor(compose_coercions arg2 arg1,
                      compose_coercions res1 res2)
  | (c1, Tcoerce_alias (env, path, c2)) ->
      Tcoerce_alias (env, path, compose_coercions c1 c2)
  | (_, _) ->
      fatal_error "Translmod.compose_coercions"

(*
let apply_coercion ~ctx a b c =
  Format.eprintf "@[<2>apply_coercion@ %a@]@." Includemod.print_coercion b;
  apply_coercion ~ctx a b c

let compose_coercions c1 c2 =
  let c3 = compose_coercions c1 c2 in
  let open Includemod in
  Format.eprintf "@[<2>compose_coercions@ (%a)@ (%a) =@ %a@]@."
    print_coercion c1 print_coercion c2 print_coercion c3;
  c3
*)

(* Record the primitive declarations occurring in the module compiled *)

let primitive_declarations = ref ([] : Primitive.description list)
let record_primitive = function
  | {val_kind=Val_prim p;val_loc} ->
      Translprim.check_primitive_arity val_loc p;
      primitive_declarations := p :: !primitive_declarations
  | _ -> ()

(* Utilities for compiling "module rec" definitions *)

let mod_prim = Lambda.transl_prim "CamlinternalMod"

let undefined_location loc =
  let (fname, line, char) = Location.get_pos_info loc.Location.loc_start in
  Lconst(Const_block(0,
                     [Const_immstring fname;
                      const_int line;
                      const_int char]))

exception Initialization_failure of unsafe_info

let init_shape id modl =
  let rec init_shape_mod path loc env mty =
    match Mtype.scrape env mty with
      Mty_ident _
    | Mty_alias _ ->
        let info = Unsafe {reason=Unsafe_module_binding;loc; path} in
        raise (Initialization_failure info)
    | Mty_signature sg ->
        Const_block(0, [Const_block(0, init_shape_struct path env sg)])
    | Mty_functor _ ->
        (* can we do better? *)
        let info = Unsafe {reason=Unsafe_functor;loc; path} in
        raise (Initialization_failure info)
  and init_shape_struct path env sg =
    match sg with
      [] -> []
    | Sig_value(subid, {val_kind=Val_reg; val_type=ty; val_loc=loc},_) :: rem ->
        let new_path = Pdot(path, Ident.name subid) in
        let init_v =
          match get_desc (Ctype.expand_head env ty) with
            Tarrow(_,_,_,_) ->
              const_int 0 (* camlinternalMod.Function *)
          | Tconstr(p, _, _) when Path.same p Predef.path_lazy_t ->
              const_int 1 (* camlinternalMod.Lazy *)
          | _ ->
              let info =
                Unsafe {reason=Unsafe_non_function; loc; path=new_path} in
              raise (Initialization_failure info)
        in
        init_v :: init_shape_struct new_path env rem
    | Sig_value(_, {val_kind=Val_prim _}, _) :: rem ->
        init_shape_struct path env rem
    | Sig_value _ :: _rem ->
        assert false
    | Sig_type(id, tdecl, _, _) :: rem ->
        init_shape_struct path (Env.add_type ~check:false id tdecl env) rem
    | Sig_typext (subid, {ext_loc=loc},_,_) :: _ ->
        let new_path = Pdot(path, Ident.name subid) in
        let info = Unsafe {reason=Unsafe_typext; loc; path=new_path} in
        raise (Initialization_failure info)
    | Sig_module(id, Mp_present, md, _, _) :: rem ->
        init_shape_mod (
          Pdot(path, Ident.name id)) md.md_loc env md.md_type ::
        init_shape_struct path (Env.add_module_declaration ~check:false
                             id Mp_present md env) rem
    | Sig_module(id, Mp_absent, md, _, _) :: rem ->
        init_shape_struct
          path (Env.add_module_declaration ~check:false
                             id Mp_absent md env) rem
    | Sig_modtype(id, minfo, _) :: rem ->
        init_shape_struct path (Env.add_modtype id minfo env) rem
    | Sig_class _ :: rem ->
        const_int 2 (* camlinternalMod.Class *)
        :: init_shape_struct path env rem
    | Sig_class_type _ :: rem ->
        init_shape_struct path env rem
  in
  try
    Ok(undefined_location modl.mod_loc,
      Lconst(
        init_shape_mod (Path.Pident id) modl.mod_loc modl.mod_env modl.mod_type)
      )
  with Initialization_failure reason -> Result.Error(reason)

(* Reorder bindings to honor dependencies.  *)

type binding_status =
  | Undefined
  | Inprogress of int option (** parent node *)
  | Defined

type id_or_ignore_loc =
  | Id of Ident.t
  | Ignore_loc of Lambda.scoped_location

let extract_unsafe_cycle id status init cycle_start =
  let info i = match init.(i) with
    | Result.Error r ->
        begin match id.(i) with
        | Id id -> id, r
        | Ignore_loc _ ->
            assert false (* Can't refer to something without a name. *)
        end
    | Ok _ -> assert false in
  let rec collect stop l i = match status.(i) with
    | Inprogress None | Undefined | Defined -> assert false
    | Inprogress Some i when i = stop -> info i :: l
    | Inprogress Some i -> collect stop (info i::l) i in
  collect cycle_start [] cycle_start

let reorder_rec_bindings bindings =
  let id = Array.of_list (List.map (fun (id,_,_,_) -> id) bindings)
  and loc = Array.of_list (List.map (fun (_,loc,_,_) -> loc) bindings)
  and init = Array.of_list (List.map (fun (_,_,init,_) -> init) bindings)
  and rhs = Array.of_list (List.map (fun (_,_,_,rhs) -> rhs) bindings) in
  let fv = Array.map Lambda.free_variables rhs in
  let num_bindings = Array.length id in
  let status = Array.make num_bindings Undefined in
  let res = ref [] in
  let is_unsafe i = match init.(i) with
    | Ok _ -> false
    | Result.Error _ -> true in
  let init_res i = match init.(i) with
    | Result.Error _ -> None
    | Ok(a,b) -> Some(a,b) in
  let rec emit_binding parent i =
    match status.(i) with
      Defined -> ()
    | Inprogress _ ->
        status.(i) <- Inprogress parent;
        let cycle = extract_unsafe_cycle id status init i in
        raise(Error(loc.(i), Circular_dependency cycle))
    | Undefined ->
        if is_unsafe i then begin
          status.(i) <- Inprogress parent;
          for j = 0 to num_bindings - 1 do
            match id.(j) with
            | Id id when Ident.Set.mem id fv.(i) -> emit_binding (Some i) j
            | _ -> ()
          done
        end;
        res := (id.(i), init_res i, rhs.(i)) :: !res;
        status.(i) <- Defined in
  for i = 0 to num_bindings - 1 do
    match status.(i) with
      Undefined -> emit_binding None i
    | Inprogress _ -> assert false
    | Defined -> ()
  done;
  List.rev !res

(* Generate lambda-code for a reordered list of bindings *)

let eval_rec_bindings bindings cont =
  let rec bind_inits = function
    [] ->
      bind_strict bindings
  | (Ignore_loc _, _, _) :: rem
  | (_, None, _) :: rem ->
      bind_inits rem
  | (Id id, Some(loc, shape), _rhs) :: rem ->
      Llet(Strict, Pgenval, id,
           Lapply{
             ap_loc=Loc_unknown;
             ap_func=mod_prim "init_mod";
             ap_args=[loc; shape];
             ap_tailcall=Default_tailcall;
             ap_inlined=Default_inline;
             ap_specialised=Default_specialise;
           },
           bind_inits rem)
  and bind_strict = function
    [] ->
      patch_forwards bindings
  | (Ignore_loc loc, None, rhs) :: rem ->
      Lsequence(Lprim(Pignore, [rhs], loc), bind_strict rem)
  | (Id id, None, rhs) :: rem ->
      Llet(Strict, Pgenval, id, rhs, bind_strict rem)
  | (_id, Some _, _rhs) :: rem ->
      bind_strict rem
  and patch_forwards = function
    [] ->
      cont
  | (Ignore_loc _, _, _rhs) :: rem
  | (_, None, _rhs) :: rem ->
      patch_forwards rem
  | (Id id, Some(_loc, shape), rhs) :: rem ->
      Lsequence(
        Lapply {
          ap_loc=Loc_unknown;
          ap_func=mod_prim "update_mod";
          ap_args=[shape; Lvar id; rhs];
          ap_tailcall=Default_tailcall;
          ap_inlined=Default_inline;
          ap_specialised=Default_specialise;
        },
        patch_forwards rem)
  in
    bind_inits bindings

let allocate_cells exported lam =
  List.fold_left
    (fun lam (_, cell) ->
       Llet (Strict, Pgenval, cell,
             Lprim (Pmakeblock (0, Mutable, None), [lambda_unit],
                    Loc_unknown),
             lam))
    lam exported

let bind_from_cells exported lam =
  List.fold_left
    (fun lam (id, cell) ->
       Llet (Strict, Pgenval, id,
             Lprim (Pfield (0, Pointer, Mutable), [Lvar cell], Loc_unknown),
             lam))
    lam exported

(* A recursive group, each binding with its translation *)
let compile_recmodule_translated ~scopes translated cont =
  eval_rec_bindings
    (reorder_rec_bindings
       (List.map
          (fun ({mb_id=id; mb_name; mb_expr=modl; _}, lam) ->
             let id_or_ignore_loc, shape =
               match id with
               | None ->
                 let loc = of_location ~scopes mb_name.loc in
                 Ignore_loc loc, Result.Error Unnamed
               | Some id -> Id id, init_shape id modl
             in
             (id_or_ignore_loc, modl.mod_loc, shape, lam))
          translated))
    cont

let compile_recmodule ~scopes compile_rhs bindings cont =
  compile_recmodule_translated ~scopes
    (List.map (fun mb -> mb, compile_rhs mb.mb_id mb.mb_expr) bindings)
    cont

(* Code to translate class entries in a structure *)

let transl_class_bindings ~ctx ~scopes cl_list =
  let ids = List.map (fun (ci, _) -> ci.ci_id_class) cl_list in
  (ids,
   List.map
     (fun ({ci_id_class=id; ci_expr=cl; ci_virt=vf}, meths) ->
       let def, rkind = transl_class ~ctx ~scopes ids id meths cl vf in
       (id, rkind, def))
     cl_list)

(* Compile one or more functors, merging curried functors to produce
   multi-argument functors.  Any [@inline] attribute on a functor that is
   merged must be consistent with any other [@inline] attribute(s) on the
   functor(s) being merged with.  Such an attribute will be placed on the
   resulting merged functor. *)

let merge_inline_attributes attr1 attr2 loc =
  match Lambda.merge_inline_attributes attr1 attr2 with
  | Some attr -> attr
  | None -> raise (Error (to_location loc, Conflicting_inline_attributes))

let merge_functors ~scopes mexp coercion root_path =
  let rec merge ~scopes mexp coercion path acc inline_attribute =
    let finished = acc, mexp, path, coercion, inline_attribute in
    match mexp.mod_desc with
    | Tmod_functor (param, body) ->
      let inline_attribute' =
        Translattribute.get_inline_attribute mexp.mod_attributes
      in
      let arg_coercion, res_coercion =
        match coercion with
        | Tcoerce_none -> Tcoerce_none, Tcoerce_none
        | Tcoerce_functor (arg_coercion, res_coercion) ->
          arg_coercion, res_coercion
        | _ -> fatal_error "Translmod.merge_functors: bad coercion"
      in
      let loc = of_location ~scopes mexp.mod_loc in
      let path, param =
        match param with
        | Unit -> None, Ident.create_local "*"
        | Named (None, _, _) ->
          let id = Ident.create_local "_" in
          functor_path path id, id
        | Named (Some id, _, _) -> functor_path path id, id
      in
      let inline_attribute =
        merge_inline_attributes inline_attribute inline_attribute' loc
      in
      merge ~scopes body res_coercion path ((param, loc, arg_coercion) :: acc)
        inline_attribute
    | _ -> finished
  in
  merge ~scopes mexp coercion root_path [] Default_inline

(* The environment of a generator holds the identifiers of its unit
   that the code it builds mentions: the run-time values that it
   quotes, and the environments of the generators of the unit that it
   calls.  A value of another unit is reached by a global path. *)
let rec address_root = function
  | Env.Aident id -> id
  | Env.Adot (a, _) -> address_root a

let expr_macro_call_env_roots e0 =
  let roots = ref Ident.Set.empty in
  let expr iter (e : Typedtree.expression) =
    (match e.exp_desc with
     | Texp_ident (path, _, desc) when desc.val_macro = Macro ->
         (match Env.find_value_env_address path e.exp_env with
          | addr ->
              let root = address_root addr in
              if not (Ident.global root) then
                roots := Ident.Set.add root !roots
          | exception Not_found -> ())
     | _ -> ());
    Tast_iterator.default_iterator.expr iter e
  in
  let iter = { Tast_iterator.default_iterator with expr } in
  iter.expr iter e0;
  !roots

let macro_call_env_roots vb = expr_macro_call_env_roots vb.vb_expr

(* The two parts of the unit find the same roots: the body is
   translated for them as in the compile-time object. *)
let macro_captured_root_set ~ctx ~scopes vb =
  let ctx = { ctx with world = Compile_world; native_slot_layout = None } in
  Ident.Set.union
    (Translcore.quoted_variables ~ctx ~scopes vb.vb_expr)
    (macro_call_env_roots vb)
  |> Ident.Set.filter (fun h -> not (Ident.global h))

let macro_captured_roots ~ctx ~scopes vb =
  Ident.Set.elements (macro_captured_root_set ~ctx ~scopes vb)

(* The quotations of a static binding are not closed over anything, so
   they may only mention the run-time values of other units.  The unit's
   own are variables here, whatever the form of the unit. *)
let check_static_quotes ~ctx ~scopes vb =
  let ctx = { ctx with native_slot_layout = None } in
  match
    Ident.Set.choose_opt (Translcore.quoted_variables ~ctx ~scopes vb.vb_expr)
  with
  | None -> ()
  | Some id -> raise (Error (vb.vb_loc, Static_quotes_local id))

let macro_bound_id vb =
  match let_bound_idents [vb] with
  | [id] -> id
  | _ -> Misc.fatal_error "Translmod: a macro binds other than one name"

(* The generators of a recursive group share one environment, so that
   one may call another with the environment it received. *)
let macro_group_captured_roots ~ctx ~scopes pat_expr_list =
  let members =
    List.fold_left (fun s vb -> Ident.Set.add (macro_bound_id vb) s)
      Ident.Set.empty pat_expr_list
  in
  List.fold_left
    (fun s vb -> Ident.Set.union s (macro_captured_root_set ~ctx ~scopes vb))
    Ident.Set.empty pat_expr_list
  |> (fun s -> Ident.Set.diff s members)
  |> Ident.Set.elements

(* The environment is the first parameter of the compile-time function,
   which is curried: compile-time code is bytecode. *)
let add_macro_env_param env_param = function
  | Lfunction lf ->
      begin match lf.kind with
      | Curried -> ()
      | Tupled ->
          Misc.fatal_error
            "Translmod: a macro's compile-time function is tupled"
      end;
      lfunction ~kind:lf.kind ~params:((env_param, Pgenval) :: lf.params)
        ~return:lf.return ~body:lf.body ~attr:lf.attr ~loc:lf.loc
  | _ -> Misc.fatal_error "Translmod: a macro is not a function"

(* Bind the run-time slot of each generator to its environment. *)
let macro_env_of_roots = function
  | [] -> Lconst const_unit
  | roots ->
      Lprim (Pmakeblock (0, Immutable, None),
             List.map (fun h -> Lvar h) roots, Loc_unknown)

let macro_env_slots ~ctx ~scopes rec_flag pat_expr_list body =
  let env_of_roots = macro_env_of_roots in
  match rec_flag with
  | Nonrecursive ->
      List.fold_right
        (fun vb body ->
           Llet (Strict, Pgenval, macro_bound_id vb,
                 env_of_roots (macro_captured_roots ~ctx ~scopes vb), body))
        pat_expr_list body
  | Recursive ->
      match macro_group_captured_roots ~ctx ~scopes pat_expr_list with
      | [] ->
          List.fold_right
            (fun vb body ->
               Llet (Strict, Pgenval, macro_bound_id vb,
                     Lconst const_unit, body))
            pat_expr_list body
      | roots ->
          match pat_expr_list with
          | [] -> assert false   (* a binding group is nonempty *)
          | first_vb :: rest ->
              let first = macro_bound_id first_vb in
              Llet (Strict, Pgenval, first, env_of_roots roots,
                    List.fold_right
                      (fun vb body ->
                         Llet (Alias, Pgenval, macro_bound_id vb,
                               Lvar first, body))
                      rest body)

(* The compile-time functions of a group of generators *)
let transl_macro_group_functions ~ctx ~scopes rec_flag pat_expr_list =
  let group =
    List.fold_left (fun s vb -> Ident.Set.add (macro_bound_id vb) s)
      Ident.Set.empty pat_expr_list
  in
  let group_index =
    match rec_flag with
    | Recursive ->
        Some (List.mapi (fun i h -> (h, [i]))
                (macro_group_captured_roots ~ctx ~scopes pat_expr_list))
    | Nonrecursive -> None
  in
  List.iter (fun vb -> Translcore.check_no_objects vb.vb_expr)
    pat_expr_list;
  List.map (fun vb ->
    let id = macro_bound_id vb in
    let me_index =
      match group_index with
      | Some index -> index
      | None ->
          List.mapi (fun i h -> (h, [i])) (macro_captured_roots ~ctx ~scopes vb)
    in
    let me_param = Ident.create_local "macro_env" in
    let fn =
      let macro_env =
        Some { Translcore.me_param; me_index; me_group = group }
      in
      transl_exp ~ctx:{ ctx with macro_env } ~scopes vb.vb_expr
    in
    (* A debug event cannot be quoted. *)
    (id, add_macro_env_param me_param (Translquote.remove_events fn)))
    pat_expr_list

(* Bind each of [ids] to the field of [block] at its position, and
   continue with [next]. *)
let rebind_fields ~scopes ~loc ~block ids fields next =
  let rec go pos fields = function
    | [] -> next fields
    | id :: ids ->
        Llet (Alias, Pgenval, id,
              Lprim (Pfield (pos, Pointer, Mutable), [Lvar block],
                     of_location ~scopes loc),
              go (pos + 1) (id :: fields) ids)
  in
  go 0 fields ids
(* Whether a module expression binds something at compile time: a
   value or a module that does, at any depth, whether or not its type
   shows it.  The compile-time part translates an item if and only if
   it binds something at compile time; otherwise the identifiers that
   the item binds are placeholders there.  With [~expand_aliases:false]
   an alias contributes nothing, and its target is not read. *)
let binds_at_compile_time ?expand_aliases (me : Typedtree.module_expr) =
  Mtype.has_macro_components ?expand_aliases me.mod_env me.mod_type
  || Typemod.has_compile_time_bindings me

(* Whether a structure item binds something at compile time; see
   [binds_at_compile_time].  An include binds the components of its
   module, whatever names it. *)
let item_binds_at_compile_time ?expand_aliases (item : Typedtree.structure_item)
  =
  let binds me = binds_at_compile_time ?expand_aliases me in
  match item.str_desc with
  | Tstr_value (_, (Macro | Static), _) -> true
  | Tstr_module mb -> binds mb.mb_expr
  | Tstr_recmodule mbs -> List.exists (fun mb -> binds mb.mb_expr) mbs
  | Tstr_include incl -> binds_at_compile_time incl.incl_mod
  | Tstr_open od -> binds od.open_expr
  | Tstr_value (_, Value, _) | Tstr_eval _ | Tstr_primitive _ | Tstr_type _
  | Tstr_typext _ | Tstr_exception _ | Tstr_modtype _ | Tstr_class _
  | Tstr_class_type _ | Tstr_attribute _ ->
      false

(* The identifiers that a structure item binds, which have slots in the
   blocks of the unit *)
let item_bound_idents (item : Typedtree.structure_item) =
  match item.str_desc with
  | Tstr_value (_, _, vbs) -> let_bound_idents vbs
  | Tstr_typext tyext ->
      List.map (fun ext -> ext.ext_id) tyext.tyext_constructors
  | Tstr_exception ext -> [ext.tyexn_constructor.ext_id]
  | Tstr_module { mb_presence = Mp_present; mb_id; _ } -> Option.to_list mb_id
  | Tstr_recmodule mbs -> List.filter_map (fun mb -> mb.mb_id) mbs
  | Tstr_class cls -> List.map (fun (ci, _) -> ci.ci_id_class) cls
  | Tstr_include incl -> bound_value_identifiers incl.incl_type
  | Tstr_open od -> bound_value_identifiers od.open_bound_items
  | Tstr_module { mb_presence = Mp_absent; _ } | Tstr_eval _
  | Tstr_primitive _ | Tstr_type _ | Tstr_modtype _ | Tstr_class_type _
  | Tstr_attribute _ -> []

(* The components of the other part of the unit keep their positions:
   the two blocks of a unit have one layout, so that the positions of a
   coercion, and those that [Env] gives, serve for both. *)
let dummy_slots ~ctx ids next fields =
  List.iter (record_compile_world_dummy ctx) ids;
  let body = next (List.rev_append ids fields) in
  List.fold_right
    (fun id body -> Llet (Alias, Pgenval, id, Lconst const_unit, body))
    ids body

let rec compile_functor ~ctx ~scopes mexp coercion root_path loc =
  let functor_params_rev, body, body_path, res_coercion, inline_attribute =
    merge_functors ~scopes mexp coercion root_path
  in
  assert (List.length functor_params_rev >= 1);  (* cf. [transl_module] *)
  let params, body =
    List.fold_left (fun (params, body) (param, loc, arg_coercion) ->
        let param' = Ident.rename param in
        let arg = apply_coercion ~ctx loc Alias arg_coercion (Lvar param') in
        let params = (param', Pgenval) :: params in
        let body = Llet (Alias, Pgenval, param, arg, body) in
        params, body)
      ([], transl_module ~ctx ~scopes res_coercion body_path body)
      functor_params_rev
  in
  lfunction
    ~kind:Curried
    ~params
    ~return:Pgenval
    ~attr:{
      inline = inline_attribute;
      specialise = Default_specialise;
      local = Default_local;
      poll = Default_poll;
      is_a_functor = true;
      stub = false;
      tmc_candidate = false;
      may_fuse_arity = true;
    }
    ~loc
    ~body

(* Compile a module expression *)

and transl_module ~ctx ~scopes cc rootpath mexp =
  begin match mexp.mod_desc with
  | (Tmod_functor _ | Tmod_apply _ | Tmod_apply_unit _)
    when in_compile_world ctx ->
      Misc.fatal_error "mixed functors: not implemented"
  | _ -> ()
  end;
  let loc = of_location ~scopes mexp.mod_loc in
  match mexp.mod_desc with
  | Tmod_ident (path,_) ->
      apply_coercion ~ctx loc Strict cc
        (transl_module_path ~ctx loc mexp.mod_env path)
  | Tmod_structure str ->
      transl_struct ~ctx ~scopes loc [] cc rootpath str
  | Tmod_functor _ ->
      oo_wrap mexp.mod_env true (fun () ->
        compile_functor ~ctx ~scopes mexp cc rootpath loc) ()
  | Tmod_apply(funct, arg, ccarg) ->
      let translated_arg = transl_module ~ctx ~scopes ccarg None arg in
      transl_apply ~ctx ~scopes ~loc ~cc mexp.mod_env funct translated_arg
  | Tmod_apply_unit funct ->
      transl_apply ~ctx ~scopes ~loc ~cc mexp.mod_env funct lambda_unit
  | Tmod_constraint(arg, _, _, ccarg) ->
      transl_module ~ctx ~scopes (compose_coercions cc ccarg) rootpath arg
  | Tmod_unpack(arg, _) ->
      apply_coercion ~ctx loc Strict cc (Translcore.transl_exp ~ctx ~scopes arg)

and transl_apply ~ctx ~scopes ~loc ~cc mod_env funct translated_arg =
  let inlined_attribute =
    Translattribute.get_inlined_attribute_on_module funct
  in
  oo_wrap mod_env true
    (apply_coercion ~ctx loc Strict cc)
    (Lapply{
       ap_loc=loc;
       ap_func=transl_module ~ctx ~scopes Tcoerce_none None funct;
       ap_args=[translated_arg];
       ap_tailcall=Default_tailcall;
       ap_inlined=inlined_attribute;
       ap_specialised=Default_specialise})

(* Whether a module expression is, through constraints, a literal
   structure. *)
and is_literal_structure me =
  match me.mod_desc with
  | Tmod_structure _ -> true
  | Tmod_constraint (me, _, _, _) -> is_literal_structure me
  | _ -> false

and transl_struct ?cont ~ctx ~scopes loc fields cc rootpath
      {str_final_env; str_items; _} =
  transl_structure ?cont ~ctx ~scopes loc fields cc rootpath str_final_env
    str_items

(* The function  transl_structure is called by  the bytecode compiler.
   Some effort is made to compile in top to bottom order, in order to display
   warning by increasing locations. *)
(* [cont] is given the block of the structure inside the scope of the
   bindings of its items. *)
and transl_structure ?cont ~ctx ~scopes loc fields cc rootpath final_env =
  function
    [] ->
      let body =
        match cc with
          Tcoerce_none ->
            Lprim(Pmakeblock(0, Immutable, None),
                  List.map (fun id -> Lvar id) (List.rev fields), loc)
        | Tcoerce_structure(pos_cc_list, id_pos_list) ->
                (* Do not ignore id_pos_list ! *)
            (*Format.eprintf "%a@.@[" Includemod.print_coercion cc;
            List.iter (fun l -> Format.eprintf "%a@ " Ident.print l)
              fields;
            Format.eprintf "@]@.";*)
            let v = Array.of_list (List.rev fields) in
            let get_field pos =
              if pos < 0 then lambda_unit
              else Lvar v.(pos)
            in
            let ids = List.fold_right Ident.Set.add fields Ident.Set.empty in
            let lam =
              Lprim(Pmakeblock(0, Immutable, None),
                  List.map
                    (fun (pos, cc) ->
                      match cc with
                        Tcoerce_primitive p ->
                          Translprim.transl_primitive
                            (of_location ~scopes p.pc_loc)
                            p.pc_desc p.pc_env p.pc_type None
                      | _ -> apply_coercion ~ctx loc Strict cc (get_field pos))
                    pos_cc_list, loc)
            and id_pos_list =
              List.filter (fun (id,_,_) -> not (Ident.Set.mem id ids))
                id_pos_list
            in
            wrap_id_pos_list ~ctx loc id_pos_list get_field lam
        | _ ->
            fatal_error "Translmod.transl_structure"
      in
      (* This debugging event provides information regarding the structure
         items. It is ignored by the OCaml debugger but is used by
         Js_of_ocaml to preserve variable names. *)
      let body =
        if !Clflags.debug && not !Clflags.native_code then
          Levent(body,
                 {lev_loc = loc;
                  lev_kind = Lev_pseudo;
                  lev_repr = None;
                  lev_env = final_env})
        else
          body
      in
      (match cont with Some k -> k body | None -> body)
  | item :: rem ->
      transl_struct_item ~ctx ~scopes fields rootpath item
        (fun fields ->
           transl_structure ?cont ~ctx ~scopes loc fields cc rootpath final_env
             rem)

(* A literal structure of the compile-time part, with the rest of the
   part, [k], inside the scope of its bindings: the code of a splice
   runs at the innermost point, and may use any of them. *)
and transl_module_hoisted ~ctx ~scopes cc rootpath me k =
  match me.mod_desc with
  | Tmod_structure str ->
      transl_struct ~cont:k ~ctx ~scopes Loc_unknown [] cc rootpath str
  | Tmod_constraint (me', _, _, ccarg) ->
      transl_module_hoisted ~ctx ~scopes (compose_coercions cc ccarg) rootpath
        me' k
  | _ -> k (transl_module ~ctx ~scopes cc rootpath me)

(* The body of a recursive module in the compile-time part.  The code at
   the innermost point of the part, after the group, uses variables that
   the body binds, and which do not reach it: each is stored in a cell at
   the innermost point of the body, to be bound again from the cell after
   the group.  The bindings of the body are renamed, so that each
   identifier is still bound once.  The result is the translation, and
   each carried variable with its cell. *)
and transl_recursive_body ~ctx ~scopes rootpath modl =
  let hole = Ident.create_local "*hole*" in
  let lam =
    transl_module_hoisted ~ctx ~scopes Tcoerce_none rootpath modl
      (fun block -> Lsequence (Lvar hole, block))
  in
  let fill code =
    Lambda.subst (fun _ _ env -> env) (Ident.Map.singleton hole code) lam
  in
  (* The variables of the innermost code that are bound at the hole *)
  let carried =
    let candidates = Ident.Set.elements ctx.innermost_free_variables in
    let free =
      free_variables (fill (make_sequence (fun v -> Lvar v) candidates))
    in
    List.filter (fun v -> not (Ident.Set.mem v free)) candidates
  in
  let cells = List.map (fun v -> v, Ident.create_local "*cell*") carried in
  let stores =
    make_sequence
      (fun (v, cell) ->
         Lprim (Psetfield (0, Pointer, Assignment), [Lvar cell; Lvar v],
                Loc_unknown))
      cells
  in
  Lambda.subst (fun _ _ env -> env) ~freshen_bound_variables:true
    Ident.Map.empty (fill stores),
  cells

and transl_struct_item ~ctx ~scopes fields rootpath item next =
  if in_compile_world ctx && not (item_binds_at_compile_time item) then
    (* The compile-time part translates an item that binds something at
       compile time.  What another binds is a placeholder there. *)
    dummy_slots ~ctx (item_bound_idents item) next fields
  else
  match item.str_desc with
  | Tstr_eval (expr, _) ->
      let body = next fields in
      Lsequence(transl_exp ~ctx ~scopes expr, body)
  | Tstr_value(rec_flag, Macro, pat_expr_list)
    when in_run_world ctx ->
      let ids = List.map macro_bound_id pat_expr_list in
      let body = next (List.rev_append ids fields) in
      macro_env_slots ~ctx ~scopes rec_flag pat_expr_list body
  | Tstr_value(rec_flag, Macro, pat_expr_list) ->
      let translated =
        transl_macro_group_functions ~ctx ~scopes rec_flag pat_expr_list
      in
      let body =
        next (List.rev_append (List.map fst translated) fields)
      in
      begin match rec_flag with
      | Nonrecursive ->
          List.fold_right
            (fun (id, fn) body -> Llet (Strict, Pgenval, id, fn, body))
            translated body
      | Recursive ->
          let rec_binding (id, fn) =
            match fn with
            | Lfunction def -> { id; def }
            | _ -> Misc.fatal_error "Translmod: a macro is not a function"
          in
          Lletrec (List.map rec_binding translated, body)
      end
  | Tstr_value(rec_flag, Static, pat_expr_list)
    when in_compile_world ctx ->
      List.iter
        (fun vb ->
           Translcore.check_no_objects vb.vb_expr;
           check_static_quotes ~ctx ~scopes vb)
        pat_expr_list;
      let mk_lam_let =
        transl_let ~ctx ~scopes ~in_structure:true rec_flag pat_expr_list in
      let ext_fields =
        List.rev_append (let_bound_idents pat_expr_list) fields in
      mk_lam_let (next ext_fields)
  | Tstr_value(_, Static, pat_expr_list) ->
      dummy_slots ~ctx (let_bound_idents pat_expr_list) next fields
  | Tstr_value(rec_flag, Value, pat_expr_list) ->
      (* Translate bindings first *)
      let mk_lam_let =
        transl_let ~ctx ~scopes ~in_structure:true rec_flag pat_expr_list in
      let ext_fields =
        List.rev_append (let_bound_idents pat_expr_list) fields in
      (* Then, translate remainder of struct *)
      let body = next ext_fields in
      mk_lam_let body
  | Tstr_primitive descr ->
      record_primitive descr.val_val;
      next fields
  | Tstr_type _ ->
      next fields
  | Tstr_typext(tyext) ->
      let ids = List.map (fun ext -> ext.ext_id) tyext.tyext_constructors in
      let body = next (List.rev_append ids fields) in
      transl_type_extension ~ctx ~scopes item.str_env rootpath tyext body
  | Tstr_exception ext ->
      let id = ext.tyexn_constructor.ext_id in
      let lam =
        transl_extension ~ctx ~scopes item.str_env rootpath
          ext.tyexn_constructor in
      let body = next (id::fields) in
      Llet(Strict, Pgenval, id, lam, body)
  | Tstr_module ({mb_presence=Mp_present} as mb)
    when in_compile_world ctx && is_literal_structure mb.mb_expr ->
      (* The rest of the part is in the scope of what the module binds;
         see [transl_module_hoisted]. *)
      let id = mb.mb_id in
      let subscopes = match id with
        | None -> scopes
        | Some id -> enter_module_definition ~scopes id in
      transl_module_hoisted ~ctx ~scopes:subscopes Tcoerce_none
        (Option.bind id (field_path rootpath)) mb.mb_expr
        (fun block ->
           let body = next (cons_opt id fields) in
           match id with
           | None ->
               Lsequence (Lprim (Pignore, [block],
                                 of_location ~scopes mb.mb_name.loc), body)
           | Some id -> Llet (Strict, Pgenval, id, block, body))
  | Tstr_module ({mb_presence=Mp_present} as mb) ->
      let id = mb.mb_id in
      (* Translate module first *)
      let subscopes = match id with
        | None -> scopes
        | Some id -> enter_module_definition ~scopes id in
      let module_body =
        transl_module ~ctx ~scopes:subscopes Tcoerce_none
          (Option.bind id (field_path rootpath)) mb.mb_expr
      in
      let module_body =
        Translattribute.add_inline_attribute module_body mb.mb_loc
          mb.mb_attributes
      in
      (* Translate remainder second *)
      let body = next (cons_opt id fields) in
      begin match id with
      | None ->
          Lsequence (Lprim(Pignore, [module_body],
                           of_location ~scopes mb.mb_name.loc), body)
      | Some id ->
          Llet(pure_module mb.mb_expr, Pgenval, id, module_body, body)
      end
  | Tstr_module ({mb_presence=Mp_absent}) ->
      next fields
  | Tstr_recmodule bindings ->
      let ext_fields =
        List.rev_append (List.filter_map (fun mb -> mb.mb_id) bindings)
          fields
      in
      let body = next ext_fields in
      let cells = ref [] in
      let translated =
        List.map
          (fun ({ mb_id; mb_expr = modl; _ } as mb) ->
             let scopes, rootpath =
               match mb_id with
               | None -> scopes, None
               | Some id ->
                   enter_module_definition ~scopes id, field_path rootpath id
             in
             if in_compile_world ctx then begin
               let lam, carried =
                 transl_recursive_body ~ctx ~scopes rootpath modl
               in
               cells := carried @ !cells;
               mb, lam
             end
             else mb, transl_module ~ctx ~scopes Tcoerce_none rootpath modl)
          bindings
      in
      allocate_cells !cells
        (compile_recmodule_translated ~scopes translated
           (bind_from_cells !cells body))
  | Tstr_class cl_list ->
      let (ids, class_bindings) = transl_class_bindings ~ctx ~scopes cl_list in
      let body = next (List.rev_append ids fields) in
      Value_rec_compiler.compile_letrec class_bindings body
  | Tstr_include incl ->
      let ids = bound_value_identifiers incl.incl_type in
      let modl = incl.incl_mod in
      let mid = Ident.create_local "include" in
      let rebind_idents fields ids =
        rebind_fields ~scopes ~loc:incl.incl_loc ~block:mid ids fields next
      in
      if in_compile_world ctx then
        transl_module_hoisted ~ctx ~scopes Tcoerce_none None modl
          (fun block ->
             (* A module that is translated only for what it binds has
                no compile-time value. *)
             if Mtype.has_macro_components modl.mod_env
                  (Mty_signature incl.incl_type)
             then Llet (Strict, Pgenval, mid, block, rebind_idents fields ids)
             else
               Lsequence (Lprim (Pignore, [block], Loc_unknown),
                          dummy_slots ~ctx ids next fields))
      else
        Llet(pure_module modl, Pgenval, mid,
             transl_module ~ctx ~scopes Tcoerce_none None modl,
             rebind_idents fields ids)
  | Tstr_open od ->
      let pure = pure_module od.open_expr in
      (* this optimization shouldn't be needed because Simplif would
         actually remove the [Llet] when it's not used.
         But since [scan_used_globals] runs before Simplif, we need to do
         it. *)
      begin match od.open_bound_items with
      | [] when pure = Alias ->
          next fields
      | _ ->
          let ids = bound_value_identifiers od.open_bound_items in
          let mid = Ident.create_local "open" in
          let rebind_idents fields ids =
            rebind_fields ~scopes ~loc:od.open_loc ~block:mid ids fields next
          in
          if in_compile_world ctx then
            transl_module_hoisted ~ctx ~scopes Tcoerce_none None od.open_expr
              (fun block ->
                 if Mtype.has_macro_components od.open_expr.mod_env
                      (Mty_signature od.open_bound_items)
                 then
                   Llet (Strict, Pgenval, mid, block,
                         rebind_idents fields ids)
                 else
                   Lsequence (Lprim (Pignore, [block], Loc_unknown),
                              dummy_slots ~ctx ids next fields))
          else
            Llet(pure, Pgenval, mid,
                 transl_module ~ctx ~scopes Tcoerce_none None od.open_expr,
                 rebind_idents fields ids)
      end
  | Tstr_modtype _
  | Tstr_class_type _
  | Tstr_attribute _ ->
      next fields

(* Update forward declaration in Translcore *)
let _ =
  Translcore.transl_module := transl_module;
  Translcore.transl_struct_item := transl_struct_item

(* Introduce dependencies on modules referenced only by "external". *)

let scan_used_globals lam =
  let is_compunit id = not (Ident.is_predef id) in
  let globals = ref Ident.Set.empty in
  let rec scan lam =
    Lambda.iter_head_constructor scan lam;
    match lam with
      Lprim ((Pgetglobal id | Psetglobal id), _, _) when (is_compunit id) ->
        globals := Ident.Set.add id !globals
    | _ -> ()
  in
  scan lam; !globals

let required_globals ~flambda body =
  let globals = scan_used_globals body in
  let add_global id req =
    if not flambda && Ident.Set.mem id globals then
      req
    else
      Ident.Set.add id req
  in
  let required =
    List.fold_left
      (fun acc path -> add_global (Path.head path) acc)
      (if flambda then globals else Ident.Set.empty)
      (Translprim.get_used_primitives ())
  in
  let required =
    List.fold_right add_global (Env.get_required_globals ()) required
  in
  Env.reset_required_globals ();
  Translprim.clear_used_primitives ();
  required

(* Compile an implementation *)

let module_block_size component_names coercion =
  match coercion with
  | Tcoerce_none -> List.length component_names
  | Tcoerce_structure (l, _) -> List.length l
  | Tcoerce_functor _
  | Tcoerce_primitive _
  | Tcoerce_alias _ -> assert false

let transl_implementation_flambda ~ctx module_name (str, cc) =
  reset_labels ();
  primitive_declarations := [];
  Translprim.clear_used_primitives ();
  let module_id = Ident.create_persistent module_name in
  let scopes = enter_module_definition ~scopes:empty_scopes module_id in
  let body =
    Translobj.transl_label_init
      (fun () -> transl_struct ~ctx ~scopes Loc_unknown [] cc
                   (global_path module_id) str)
  in
  let size =
    module_block_size (bound_value_identifiers str.str_type) cc in
  { module_ident = module_id;
    main_module_block_size = size;
    required_globals = required_globals ~flambda:true body;
    code = body }

let transl_implementation ~ctx module_name (str, cc) =
  let implementation =
    transl_implementation_flambda ~ctx module_name (str, cc)
  in
  let code =
    Lprim (Psetglobal implementation.module_ident, [implementation.code],
           Loc_unknown)
  in
  { implementation with code }

(* The globals that typing records are those of the run-time part.  Run
   a translation of the compile-time part without them. *)
let without_typing_required_globals f =
  let saved = Env.get_required_globals () in
  Env.reset_required_globals ();
  let result = f () in
  List.iter Env.add_required_global saved;
  result

let macros_global module_name =
  Ident.create_persistent (Unit_info.macros_modname module_name)

(* Whether a structure exports anything that it binds at compile time.
   A module alias is not expanded: under -no-alias-deps its target is not
   read.  What an open binds is not exported. *)
let exports_macros str =
  List.exists
    (fun item -> match item.str_desc with
       | Tstr_open _ -> false
       | _ -> item_binds_at_compile_time ~expand_aliases:false item)
    str.str_items

(* The compile-time part of a unit: the global [M$macros], a block with
   the layout of the block of the unit.  It refers to no run-time
   object, and it is bytecode, whichever compiler builds it. *)
let transl_macros_object module_name (str, cc) =
  protect_refs [R (Clflags.native_code, false)] @@ fun () ->
  let module_id = Ident.create_persistent module_name in
  let macros_id = macros_global module_name in
  let scopes = enter_module_definition ~scopes:empty_scopes module_id in
  let ctx = initial_context () in
  let body, required =
    without_typing_required_globals @@ fun () ->
      let body =
        in_world ctx Compile_world @@ fun ctx ->
          let mod_body =
            transl_struct ~ctx ~scopes Loc_unknown [] cc
              (Some (Path.Pident module_id)) str
          in
          Lprim (Psetglobal macros_id, [mod_body], Loc_unknown)
      in
      body, required_globals ~flambda:false body
  in
  { module_ident = macros_id;
    main_module_block_size =
      module_block_size (bound_value_identifiers str.str_type) cc;
    required_globals = required;
    code = body }

(* Build the list of value identifiers defined by a toplevel structure
   (excluding primitive declarations). *)

let rec defined_idents = function
    [] -> []
  | item :: rem ->
    match item.str_desc with
    | Tstr_eval _ -> defined_idents rem
    | Tstr_value(_rec_flag, _, pat_expr_list) ->
      let_bound_idents pat_expr_list @ defined_idents rem
    | Tstr_primitive _ -> defined_idents rem
    | Tstr_type _ -> defined_idents rem
    | Tstr_typext tyext ->
      List.map (fun ext -> ext.ext_id) tyext.tyext_constructors
      @ defined_idents rem
    | Tstr_exception ext -> ext.tyexn_constructor.ext_id :: defined_idents rem
    | Tstr_module {mb_id = Some id; mb_presence=Mp_present} ->
      id :: defined_idents rem
    | Tstr_module ({mb_id = None}
                  |{mb_presence=Mp_absent}) -> defined_idents rem
    | Tstr_recmodule decls ->
      List.filter_map (fun mb -> mb.mb_id) decls @ defined_idents rem
    | Tstr_modtype _ -> defined_idents rem
    | Tstr_open od ->
      bound_value_identifiers od.open_bound_items @ defined_idents rem
    | Tstr_class cl_list ->
      List.map (fun (ci, _) -> ci.ci_id_class) cl_list @ defined_idents rem
    | Tstr_class_type _ -> defined_idents rem
    | Tstr_include incl ->
      bound_value_identifiers incl.incl_type @ defined_idents rem
    | Tstr_attribute _ -> defined_idents rem

(* second level idents (module M = struct ... let id = ... end),
   and all sub-levels idents *)
let rec more_idents = function
    [] -> []
  | item :: rem ->
    match item.str_desc with
    | Tstr_eval _ -> more_idents rem
    | Tstr_value _ -> more_idents rem
    | Tstr_primitive _ -> more_idents rem
    | Tstr_type _ -> more_idents rem
    | Tstr_typext _ -> more_idents rem
    | Tstr_exception _ -> more_idents rem
    | Tstr_recmodule _ -> more_idents rem
    | Tstr_modtype _ -> more_idents rem
    | Tstr_open od ->
        let rest = more_idents rem in
        begin match od.open_expr.mod_desc with
        | Tmod_structure str -> all_idents str.str_items @ rest
        | _ -> rest
        end
    | Tstr_class _ -> more_idents rem
    | Tstr_class_type _ -> more_idents rem
    | Tstr_include{incl_mod={mod_desc =
                             Tmod_constraint ({mod_desc = Tmod_structure str},
                                              _, _, _)
                            | Tmod_structure str }} ->
        all_idents str.str_items @ more_idents rem
    | Tstr_include _ -> more_idents rem
    | Tstr_module
        {mb_presence=Mp_present; mb_expr={mod_desc = Tmod_structure str}}
    | Tstr_module
        {mb_presence=Mp_present;
         mb_expr={mod_desc=
           Tmod_constraint ({mod_desc = Tmod_structure str}, _, _, _)}} ->
        all_idents str.str_items @ more_idents rem
    | Tstr_module _ -> more_idents rem
    | Tstr_attribute _ -> more_idents rem

and all_idents = function
    [] -> []
  | item :: rem ->
    match item.str_desc with
    | Tstr_eval _ -> all_idents rem
    | Tstr_value(_rec_flag, _, pat_expr_list) ->
      let_bound_idents pat_expr_list @ all_idents rem
    | Tstr_primitive _ -> all_idents rem
    | Tstr_type _ -> all_idents rem
    | Tstr_typext tyext ->
      List.map (fun ext -> ext.ext_id) tyext.tyext_constructors
      @ all_idents rem
    | Tstr_exception ext -> ext.tyexn_constructor.ext_id :: all_idents rem
    | Tstr_recmodule decls ->
      List.filter_map (fun mb -> mb.mb_id) decls @ all_idents rem
    | Tstr_modtype _ -> all_idents rem
    | Tstr_open od ->
        let rest = all_idents rem in
        begin match od.open_expr.mod_desc with
        | Tmod_structure str ->
          bound_value_identifiers od.open_bound_items
          @ all_idents str.str_items
          @ rest
        | _ -> bound_value_identifiers od.open_bound_items @ rest
        end
    | Tstr_class cl_list ->
      List.map (fun (ci, _) -> ci.ci_id_class) cl_list @ all_idents rem
    | Tstr_class_type _ -> all_idents rem

    | Tstr_include{incl_type;
                   incl_mod={mod_desc =
                     ( Tmod_constraint({mod_desc=Tmod_structure str}, _, _, _)
                     | Tmod_structure str )}} ->
        bound_value_identifiers incl_type
        @ all_idents str.str_items
        @ all_idents rem
    | Tstr_include incl ->
      bound_value_identifiers incl.incl_type @ all_idents rem

    | Tstr_module
        { mb_id = Some id;
          mb_presence=Mp_present;
          mb_expr={mod_desc = Tmod_structure str} }
    | Tstr_module
        { mb_id = Some id;
          mb_presence = Mp_present;
          mb_expr =
            {mod_desc =
               Tmod_constraint ({mod_desc = Tmod_structure str}, _, _, _)}} ->
        id :: all_idents str.str_items @ all_idents rem
    | Tstr_module {mb_id = Some id;mb_presence=Mp_present} ->
        id :: all_idents rem
    | Tstr_module ({mb_id = None} | {mb_presence=Mp_absent}) -> all_idents rem
    | Tstr_attribute _ -> all_idents rem


(* A variant of transl_structure used to compile toplevel structure definitions
   for the native-code compiler. Store the defined values in the fields
   of the global as soon as they are defined, in order to reduce register
   pressure.  Also rewrites the defining expressions so that they
   refer to earlier fields of the structure through the fields of
   the global, not by their names.
   "map" is a table from defined idents to (pos in global block, coercion).
   "prim" is a list of (pos in global block, primitive declaration). *)

let transl_store_subst = ref Ident.Map.empty
  (** In the native toplevel, this reference is threaded through successive
      calls of transl_store_structure *)

let nat_toplevel_name id =
  try match Ident.Map.find id !transl_store_subst with
    | Lprim(Pfield (pos, _, _),
            [Lprim(Pgetglobal glob, [], _)], _) -> (glob,pos)
    | _ -> raise Not_found
  with Not_found ->
    fatal_error("Translmod.nat_toplevel_name: " ^ Ident.unique_name id)

let field_of_str ~ctx loc str =
  let ids = Array.of_list (defined_idents str.str_items) in
  fun (pos, cc) ->
    match cc with
    | Tcoerce_primitive { pc_loc = _; pc_desc; pc_env; pc_type; } ->
        Translprim.transl_primitive loc pc_desc pc_env pc_type None
    | Tcoerce_alias (env, path, cc) ->
        let lam = transl_module_path ~ctx loc env path in
        apply_coercion ~ctx loc Alias cc lam
    | _ -> apply_coercion ~ctx loc Strict cc (Lvar ids.(pos))


(* Within a recursive group, a module of the group is a variable in
   the store form too, as a direct reference to it is: quoted code
   closes over it, and the order of evaluation of the group sees the
   dependency. *)
let group_modules_as_variables ctx ids =
  match ctx.native_slot_layout with
  | None -> ctx
  | Some nsl ->
      let nsl_pos id =
        if List.exists (Ident.same id) ids then None else nsl.nsl_pos id
      in
      { ctx with native_slot_layout = Some { nsl with nsl_pos } }

let transl_store_structure ~ctx ~scopes glob map prims aliases str =
  let no_env_update _ _ env = env in
  let rec transl_store ~scopes rootpath subst cont = function
    [] ->
      transl_store_subst := subst;
      Lambda.subst no_env_update subst cont
    | item :: rem ->
        match item.str_desc with
        | Tstr_eval (expr, _attrs) ->
            Lsequence(Lambda.subst no_env_update subst
                        (transl_exp ~ctx ~scopes expr),
                      transl_store ~scopes rootpath subst cont rem)
        | Tstr_value(rec_flag, Macro, pat_expr_list) ->
            let ids = List.map macro_bound_id pat_expr_list in
            let lam =
              macro_env_slots ~ctx ~scopes rec_flag pat_expr_list
                (store_idents Loc_unknown ids)
            in
            Lsequence(Lambda.subst no_env_update subst lam,
                      transl_store ~scopes rootpath
                        (add_idents false ids subst) cont rem)
        | Tstr_value(_, Static, pat_expr_list) ->
            let ids = let_bound_idents pat_expr_list in
            let lam =
              List.fold_right
                (fun id body ->
                   Llet (Alias, Pgenval, id, Lconst const_unit, body))
                ids
                (store_idents Loc_unknown ids)
            in
            Lsequence(Lambda.subst no_env_update subst lam,
                      transl_store ~scopes rootpath
                        (add_idents false ids subst) cont rem)
        | Tstr_value(rec_flag, Value, pat_expr_list) ->
            let ids = let_bound_idents pat_expr_list in
            let lam =
              transl_let ~ctx ~scopes ~in_structure:true rec_flag pat_expr_list
                (store_idents Loc_unknown ids)
            in
            Lsequence(Lambda.subst no_env_update subst lam,
                      transl_store ~scopes rootpath
                        (add_idents false ids subst) cont rem)
        | Tstr_primitive descr ->
            record_primitive descr.val_val;
            transl_store ~scopes rootpath subst cont rem
        | Tstr_type _ ->
            transl_store ~scopes rootpath subst cont rem
        | Tstr_typext(tyext) ->
            let ids =
              List.map (fun ext -> ext.ext_id) tyext.tyext_constructors
            in
            let lam =
              transl_type_extension ~ctx ~scopes item.str_env rootpath tyext
                                    (store_idents Loc_unknown ids)
            in
            Lsequence(Lambda.subst no_env_update subst lam,
                      transl_store ~scopes rootpath
                        (add_idents false ids subst) cont rem)
        | Tstr_exception ext ->
            let id = ext.tyexn_constructor.ext_id in
            let path = field_path rootpath id in
            let loc = of_location ~scopes ext.tyexn_constructor.ext_loc in
            let lam =
              transl_extension_constructor ~scopes
                                           item.str_env
                                           path
                                           ext.tyexn_constructor
            in
            Lsequence(Llet(Strict, Pgenval, id,
                           Lambda.subst no_env_update subst lam,
                           store_ident loc id),
                      transl_store ~scopes rootpath
                        (add_ident false id subst) cont rem)
        | Tstr_module
            {mb_id=None; mb_name; mb_presence=Mp_present; mb_expr=modl;
             mb_loc=loc; mb_attributes} ->
            let lam =
              Translattribute.add_inline_attribute
                (transl_module ~ctx ~scopes Tcoerce_none None modl)
                loc mb_attributes
            in
            Lsequence(
              Lprim(Pignore,[Lambda.subst no_env_update subst lam],
                    of_location ~scopes mb_name.loc),
              transl_store ~scopes rootpath subst cont rem
            )
        | Tstr_module{mb_id=Some id;mb_loc=loc;mb_presence=Mp_present;
                      mb_expr={mod_desc = Tmod_structure str}} ->
            let loc = of_location ~scopes loc in
            let lam =
              transl_store
                ~scopes:(enter_module_definition ~scopes id)
                (field_path rootpath id) subst
                lambda_unit str.str_items
            in
            (* Careful: see next case *)
            let subst = !transl_store_subst in
            Lsequence(lam,
                      Llet(Strict, Pgenval, id,
                           Lambda.subst no_env_update subst
                             (Lprim(Pmakeblock(0, Immutable, None),
                                    List.map (fun id -> Lvar id)
                                      (defined_idents str.str_items), loc)),
                           Lsequence(store_ident loc id,
                                     transl_store ~scopes rootpath
                                                  (add_ident true id subst)
                                                  cont rem)))
        | Tstr_module{
            mb_id=Some id;mb_loc=loc;mb_presence=Mp_present;
            mb_expr= {
              mod_desc = Tmod_constraint (
                  {mod_desc = Tmod_structure str}, _, _,
                  (Tcoerce_structure (map, _) as _cc))}
          } ->
            (*    Format.printf "coerc id %s: %a@." (Ident.unique_name id)
                                Includemod.print_coercion cc; *)
            let loc = of_location ~scopes loc in
            let lam =
              transl_store
                ~scopes:(enter_module_definition ~scopes id)
                (field_path rootpath id) subst
                lambda_unit str.str_items
            in
            (* Careful: see next case *)
            let subst = !transl_store_subst in
            let field = field_of_str ~ctx loc str in
            Lsequence(lam,
                      Llet(Strict, Pgenval, id,
                           Lambda.subst no_env_update subst
                             (Lprim(Pmakeblock(0, Immutable, None),
                                    List.map field map, loc)),
                           Lsequence(store_ident loc id,
                                     transl_store ~scopes rootpath
                                                  (add_ident true id subst)
                                                  cont rem)))
        | Tstr_module
            {mb_id=Some id; mb_presence=Mp_present; mb_expr=modl;
             mb_loc=loc; mb_attributes} ->
            let lam =
              Translattribute.add_inline_attribute
                (transl_module
                   ~ctx ~scopes:(enter_module_definition ~scopes id)
                   Tcoerce_none (field_path rootpath id) modl)
                loc mb_attributes
            in
            (* Careful: the module value stored in the global may be different
               from the local module value, in case a coercion is applied.
               If so, keep using the local module value (id) in the remainder of
               the compilation unit (add_ident true returns subst unchanged).
               If not, we can use the value from the global
               (add_ident true adds id -> Pgetglobal... to subst). *)
            Llet(Strict, Pgenval, id, Lambda.subst no_env_update subst lam,
                 Lsequence(store_ident (of_location ~scopes loc) id,
                           transl_store ~scopes rootpath
                             (add_ident true id subst)
                             cont rem))
        | Tstr_module ({mb_presence=Mp_absent}) ->
            transl_store ~scopes rootpath subst cont rem
        | Tstr_recmodule bindings ->
            let ids = List.filter_map (fun mb -> mb.mb_id) bindings in
            let ctx = group_modules_as_variables ctx ids in
            compile_recmodule ~scopes
              (fun id modl ->
                 Lambda.subst no_env_update subst
                   (match id with
                    | None ->
                      transl_module ~ctx ~scopes Tcoerce_none None modl
                    | Some id ->
                      transl_module
                        ~ctx ~scopes:(enter_module_definition ~scopes id)
                        Tcoerce_none (field_path rootpath id) modl))
              bindings
              (Lsequence(store_idents Loc_unknown ids,
                         transl_store ~scopes rootpath
                           (add_idents true ids subst) cont rem))
        | Tstr_class cl_list ->
            let (ids, class_bindings) =
              transl_class_bindings ~ctx ~scopes cl_list
            in
            let lam =
              Value_rec_compiler.compile_letrec class_bindings
                (store_idents Loc_unknown ids)
            in
            Lsequence(Lambda.subst no_env_update subst lam,
                      transl_store ~scopes rootpath (add_idents false ids subst)
                        cont rem)

        | Tstr_include({
            incl_loc=loc;
            incl_mod= {
              mod_desc = Tmod_constraint (
                  ({mod_desc = Tmod_structure str}), _, _,
                  (Tcoerce_structure _ | Tcoerce_none))}
            | ({ mod_desc = Tmod_structure str});
            incl_type;
          } as incl) ->
            let lam =
              transl_store ~scopes None subst lambda_unit str.str_items
                (* It is tempting to pass rootpath instead of None
                   in order to give a more precise name to exceptions
                   in the included structured, but this would introduce
                   a difference of behavior compared to bytecode. *)
            in
            let subst = !transl_store_subst in
            let field = field_of_str ~ctx (of_location ~scopes loc) str in
            let ids0 = bound_value_identifiers incl_type in
            let rec loop ids args =
              match ids, args with
              | [], [] ->
                  transl_store ~scopes rootpath (add_idents true ids0 subst)
                    cont rem
              | id :: ids, arg :: args ->
                  Llet(Alias, Pgenval, id,
                       Lambda.subst no_env_update subst (field arg),
                       Lsequence(store_ident (of_location ~scopes loc) id,
                                 loop ids args))
              | _ -> assert false
            in
            let map =
              match incl.incl_mod.mod_desc with
              | Tmod_constraint (_, _, _, Tcoerce_structure (map, _)) ->
                 map
              | Tmod_structure _
              | Tmod_constraint (_, _, _, Tcoerce_none) ->
                 List.init (List.length ids0) (fun i -> i, Tcoerce_none)
              | _ -> assert false
            in
            Lsequence(lam, loop ids0 map)

        | Tstr_include incl ->
            let ids = bound_value_identifiers incl.incl_type in
            let modl = incl.incl_mod in
            let mid = Ident.create_local "include" in
            let loc = incl.incl_loc in
            let rec store_idents pos = function
              | [] -> transl_store
                        ~scopes rootpath (add_idents true ids subst) cont rem
              | id :: idl ->
                  Llet(Alias, Pgenval, id,
                       Lprim(Pfield (pos, Pointer, Mutable), [Lvar mid],
                                                 of_location ~scopes loc),
                       Lsequence(store_ident (of_location ~scopes loc) id,
                                 store_idents (pos + 1) idl))
            in
            Llet(Strict, Pgenval, mid,
                 Lambda.subst no_env_update subst
                   (transl_module ~ctx ~scopes Tcoerce_none None modl),
                 store_idents 0 ids)
        | Tstr_open od ->
            begin match od.open_expr.mod_desc with
            | Tmod_structure str ->
                let lam =
                  transl_store ~scopes rootpath subst lambda_unit str.str_items
                in
                let loc = of_location ~scopes od.open_loc in
                let ids = Array.of_list (defined_idents str.str_items) in
                let ids0 = bound_value_identifiers od.open_bound_items in
                let subst = !transl_store_subst in
                let rec store_idents pos = function
                  | [] -> transl_store ~scopes rootpath
                            (add_idents true ids0 subst) cont rem
                  | id :: idl ->
                      Llet(Alias, Pgenval, id, Lvar ids.(pos),
                           Lsequence(store_ident loc id,
                                     store_idents (pos + 1) idl))
                in
                Lsequence(lam, Lambda.subst no_env_update subst
                                 (store_idents 0 ids0))
            | _ ->
                let pure = pure_module od.open_expr in
                (* this optimization shouldn't be needed because Simplif would
                   actually remove the [Llet] when it's not used.
                   But since [scan_used_globals] runs before Simplif, we need to
                   do it. *)
                match od.open_bound_items with
                | [] when pure = Alias ->
                  transl_store ~scopes rootpath subst cont rem
                | _ ->
                    let ids = bound_value_identifiers od.open_bound_items in
                    let mid = Ident.create_local "open" in
                    let loc = of_location ~scopes od.open_loc in
                    let rec store_idents pos = function
                        [] -> transl_store ~scopes rootpath
                                (add_idents true ids subst) cont rem
                      | id :: idl ->
                          Llet(Alias, Pgenval, id,
                               Lprim(Pfield (pos, Pointer, Mutable),
                                     [Lvar mid], loc),
                               Lsequence(store_ident loc id,
                                         store_idents (pos + 1) idl))
                    in
                    Llet(
                      pure, Pgenval, mid,
                      Lambda.subst no_env_update subst
                        (transl_module ~ctx ~scopes Tcoerce_none None
                           od.open_expr),
                      store_idents 0 ids)
          end
        | Tstr_modtype _
        | Tstr_class_type _
        | Tstr_attribute _ ->
            transl_store ~scopes rootpath subst cont rem

  and store_ident loc id =
    try
      let (pos, cc) = Ident.find_same id map in
      let init_val = apply_coercion ~ctx loc Alias cc (Lvar id) in
      Lprim(Psetfield(pos, Pointer, Root_initialization),
            [Lprim(Pgetglobal glob, [], loc); init_val],
            loc)
    with Not_found ->
      fatal_error("Translmod.store_ident: " ^ Ident.unique_name id)

  and store_idents loc idlist =
    make_sequence (store_ident loc) idlist

  and add_ident may_coerce id subst =
    try
      let (pos, cc) = Ident.find_same id map in
      match cc with
        Tcoerce_none ->
          Ident.Map.add id
            (Lprim(Pfield (pos, Pointer, Immutable),
                   [Lprim(Pgetglobal glob, [], Loc_unknown)],
                   Loc_unknown))
            subst
      | _ ->
          if may_coerce then subst else assert false
    with Not_found ->
      assert false

  and add_idents may_coerce idlist subst =
    List.fold_right (add_ident may_coerce) idlist subst

  and store_primitive (pos, prim) cont =
    Lsequence(Lprim(Psetfield(pos, Pointer, Root_initialization),
                    [Lprim(Pgetglobal glob, [], Loc_unknown);
                     Translprim.transl_primitive Loc_unknown
                       prim.pc_desc prim.pc_env prim.pc_type None],
                    Loc_unknown),
              cont)

  and store_alias (pos, env, path, cc) =
    let path_lam = transl_module_path ~ctx Loc_unknown env path in
    let init_val = apply_coercion ~ctx Loc_unknown Strict cc path_lam in
    Lprim(Psetfield(pos, Pointer, Root_initialization),
          [Lprim(Pgetglobal glob, [], Loc_unknown);
           init_val],
          Loc_unknown)
  in
  let aliases = make_sequence store_alias aliases in
  List.fold_right store_primitive prims
    (transl_store ~scopes (global_path glob) !transl_store_subst aliases str)

(* Transform a coercion and the list of value identifiers defined by
   a toplevel structure into a table [id -> (pos, coercion)],
   with [pos] being the position in the global block where the value of
   [id] must be stored, and [coercion] the coercion to be applied to it.
   A given identifier may appear several times
   in the coercion (if it occurs several times in the signature); remember
   to assign it the position of its last occurrence.
   Identifiers that are not exported are assigned positions at the
   end of the block (beyond the positions of all exported idents).
   Also compute the total size of the global block,
   and the list of all primitives exported as values. *)

let build_ident_map restr idlist more_ids =
  let rec natural_map pos map prims aliases = function
    | [] ->
        (map, prims, aliases, pos)
    | id :: rem ->
        natural_map (pos+1)
          (Ident.add id (pos, Tcoerce_none) map) prims aliases rem
  in
  let (map, prims, aliases, pos) =
    match restr with
    | Tcoerce_none ->
        natural_map 0 Ident.empty [] [] idlist
    | Tcoerce_structure (pos_cc_list, _id_pos_list) ->
        (* ignore _id_pos_list as the ids are already bound *)
        let idarray = Array.of_list idlist in
        let rec export_map pos map prims aliases undef = function
          | [] ->
              natural_map pos map prims aliases undef
          | (_source_pos, Tcoerce_primitive p) :: rem ->
              export_map (pos + 1) map
                ((pos, p) :: prims) aliases undef rem
          | (_source_pos, Tcoerce_alias(env, path, cc)) :: rem ->
              export_map (pos + 1) map prims
                ((pos, env, path, cc) :: aliases) undef rem
          | (source_pos, cc) :: rem ->
              let id = idarray.(source_pos) in
              export_map (pos + 1) (Ident.add id (pos, cc) map)
                prims aliases (list_remove id undef) rem
        in
        export_map 0 Ident.empty [] [] idlist pos_cc_list
    | _ ->
        fatal_error "Translmod.build_ident_map"
  in
  natural_map pos map prims aliases more_ids

(* The field of each identifier of a unit in the store form; [None] for
   one stored under a coercion. *)
let store_ident_positions ({ str_items = str; _ }, restr) =
  let (map, _prims, _aliases, _size) =
    build_ident_map restr (defined_idents str) (more_idents str)
  in
  fun id ->
    match Ident.find_same id map with
    | (pos, Tcoerce_none) -> Some pos
    | _ -> None
    | exception Not_found -> None

(* Compile an implementation using transl_store_structure
   (for the native-code compiler). *)

let transl_store_gen ~ctx ~scopes module_name ({ str_items = str }, restr)
    topl =
  reset_labels ();
  primitive_declarations := [];
  Translprim.clear_used_primitives ();
  let module_id = Ident.create_persistent module_name in
  let (map, prims, aliases, size) =
    build_ident_map restr (defined_idents str) (more_idents str) in
  let f = function
    | [ { str_desc = Tstr_eval (expr, _attrs) } ] when topl ->
        assert (size = 0);
        Lambda.subst (fun _ _ env -> env) !transl_store_subst
          (transl_exp ~ctx ~scopes expr)
    | str -> transl_store_structure ~ctx ~scopes module_id map prims aliases str
  in
  transl_store_label_init module_id size f str
  (*size, transl_label_init (transl_store_structure module_id map prims str)*)

let transl_store_phrases ~ctx module_name str =
  let scopes =
    enter_module_definition ~scopes:empty_scopes
      (Ident.create_persistent module_name)
  in
  transl_store_gen ~ctx ~scopes module_name (str,Tcoerce_none) true

let transl_store_implementation ~ctx module_name (str, restr) =
  let s = !transl_store_subst in
  transl_store_subst := Ident.Map.empty;
  let module_ident = Ident.create_persistent module_name in
  let scopes = enter_module_definition ~scopes:empty_scopes module_ident in
  let (i, code) =
    transl_store_gen ~ctx ~scopes module_name (str, restr) false
  in
  transl_store_subst := s;
  { Lambda.main_module_block_size = i;
    code;
    (* module_ident is not used by closure, but this allow to share
       the type with the flambda version *)
    module_ident;
    required_globals = required_globals ~flambda:true code }

(* A function of the compiler that the static program calls.  The
   function is the only component of the unit [m], which the program
   that runs the static program is linked with. *)
let call_static_support m args =
  let unit = Lprim (Pgetglobal (Ident.create_persistent m), [], Loc_unknown) in
  Lapply { ap_func = Lprim (Pfield (0, Pointer, Immutable), [unit],
                            Loc_unknown);
           ap_args = args;
           ap_loc = Loc_unknown;
           ap_tailcall = Default_tailcall;
           ap_inlined = Default_inline;
           ap_specialised = Default_specialise }

(* The code of each top-level splice is a function, which the hole of
   the splice applies to the variables of the run-time term that the
   splice quotes: a splice is evaluated when the term around it is
   built.  [translate] gives the run-time term.  The second result binds
   the functions around the code that builds the term. *)
let with_splice_thunks ~ctx ~scopes ?(report = fun _ code -> code)
      translate =
  let pending_splices = ref [] in
  let splice_thunk ctx spl_exp =
    (* Compile-time code is bytecode, whichever compiler builds it. *)
    protect_refs [R (Clflags.native_code, false)] @@ fun () ->
    in_world ctx Compile_world @@ fun ctx ->
    Translcore.check_no_objects spl_exp;
    (* A variable that is a field of the block of the unit is quoted as
       that field, and is not closed over. *)
    let roots = Translcore.quoted_variables ~ctx ~scopes spl_exp in
    let originals, fresh_vars, code =
      Translquote.transl_close_function Location.none roots
        (fun closing ->
           Translcore.transl_exp ~ctx:{ ctx with closing } ~scopes spl_exp)
    in
    let code = report spl_exp.exp_loc code in
    let h = Ident.create_local "*splice*" in
    (* Without variables, the function is a thunk, which the hole
       applies: the splice is evaluated there all the same. *)
    let params, sp_code =
      match fresh_vars with
      | [] ->
          [ (Ident.create_local "*unit*", Pgenval) ],
          Lapply { ap_func = Lvar h; ap_args = [ lambda_unit ];
                   ap_loc = Loc_unknown; ap_tailcall = Default_tailcall;
                   ap_inlined = Default_inline;
                   ap_specialised = Default_specialise }
      | _ -> List.map (fun v -> (v, Pgenval)) fresh_vars, Lvar h
    in
    let fn =
      lfunction ~kind:Curried ~params ~return:Pgenval ~body:code
        ~attr:default_function_attribute ~loc:Loc_unknown
    in
    pending_splices := (h, fn) :: !pending_splices;
    { sp_code; sp_args = originals }
  in
  let run_time = translate { ctx with splice_source = Some splice_thunk } in
  let bind_thunks body =
    List.fold_right
      (fun (h, code) body -> Llet (Strict, Pgenval, h, code, body))
      (List.rev !pending_splices) body
  in
  run_time, bind_thunks

(* The static program of a unit with top-level splices, and the run-time
   term of the unit, with a hole for each top-level splice.

   The static program is the compile-time part of the unit with, at its
   innermost point, code that evaluates the code of each splice and
   writes the terms that fill the holes; the compiler fills the holes
   of its own term with them, and compiles it.  The term is not
   simplified before because a hole hides the binders that its contents
   mention. *)
let transl_static_program ?(native = false) module_name (str, cc)
    ~source_file ~prefix =
  let module_id = Ident.create_persistent module_name in
  let scopes = enter_module_definition ~scopes:empty_scopes module_id in
  (* A failure of an evaluation is written to [err_file], where the
     driver finds it. *)
  let err_file = prefix ^ "$static.err" in
  let static_report loc what body =
    call_static_support "Static_report"
      [Translquote.const err_file;
       Translquote.const (loc : Location.t);
       Translquote.const what;
       Lambda.thunk (Ident.create_local "*report*") body]
  in
  let slot_layout =
    if native && not Config.flambda then
      Some { Translcore.nsl_module = module_id;
             nsl_pos = store_ident_positions (str, cc) }
    else None
  in
  let ctx = { (initial_context ()) with native_slot_layout = slot_layout } in
  without_typing_required_globals @@ fun () ->
  let run_time, bind_splice_thunks =
    with_splice_thunks ~ctx ~scopes
      ~report:(fun loc code -> static_report loc "this splice" code)
      (fun ctx ->
         if not native then transl_implementation ~ctx module_name (str, cc)
         else if Config.flambda then
           transl_implementation_flambda ~ctx module_name (str, cc)
         else transl_store_implementation ~ctx module_name (str, cc))
  in
  (* The globals that the object requires are those of the term with
     its holes empty: the globals of a splice's code belong to the
     static program. *)
  let rt_required_globals =
    let rec blank lam =
      match lam with
      | Lsplice _ -> lsplice lambda_unit
      | _ -> Lambda.shallow_map blank lam
    in
    required_globals ~flambda:false (blank run_time.code)
  in
  let emit =
    bind_splice_thunks
      (call_static_support "Static_holes"
         [Translquote.const (prefix ^ "$static.holes");
          Translquote.quote_module_holes run_time.code])
  in
  let static_id = Ident.create_persistent (module_name ^ "$static") in
  (* The call that writes the code of the holes is inside the scope of
     every binding of the compile-time part. *)
  let code =
    protect_refs [R (Clflags.native_code, false)] @@ fun () ->
    let ctx = { ctx with innermost_free_variables = free_variables emit } in
    in_world ctx Compile_world @@ fun ctx ->
      transl_struct ~ctx ~scopes Loc_unknown [] cc
        (Some (Path.Pident module_id)) str
        ~cont:(fun macros_block ->
          Lsequence
            (Lprim (Psetglobal (macros_global module_name), [macros_block],
                    Loc_unknown),
             Lsequence
               (emit,
                Lprim (Psetglobal static_id,
                       [Lprim (Pmakeblock (0, Immutable, None),
                               [Lconst const_unit], Loc_unknown)],
                       Loc_unknown))))
  in
  (* A failure outside any splice is attributed to the unit. *)
  let code =
    static_report (Location.in_file source_file)
      "the compile-time part of this unit" code
  in
  { module_ident = static_id;
    main_module_block_size = 1;
    required_globals = required_globals ~flambda:false code;
    code },
  { run_time with required_globals = rt_required_globals }

(* Compile a toplevel phrase *)

let toploop_ident = Ident.create_persistent "Toploop"
let toploop_getvalue_pos = 0 (* position of getvalue in module Toploop *)
let toploop_setvalue_pos = 1 (* position of setvalue in module Toploop *)

let aliased_idents = ref Ident.empty

let set_toplevel_unique_name id =
  aliased_idents :=
    Ident.add id (Ident.unique_toplevel_name id) !aliased_idents

let toplevel_name id =
  try Ident.find_same id !aliased_idents
  with Not_found -> Ident.name id

let toploop_getvalue_key key =
  Lapply{
    ap_loc=Loc_unknown;
    ap_func=Lprim(Pfield (toploop_getvalue_pos, Pointer, Mutable),
                  [Lprim(Pgetglobal toploop_ident, [], Loc_unknown)],
                  Loc_unknown);
    ap_args=[Lconst(Const_immstring key)];
    ap_tailcall=Default_tailcall;
    ap_inlined=Default_inline;
    ap_specialised=Default_specialise;
  }

let toploop_getvalue id = toploop_getvalue_key (toplevel_name id)

let toploop_setvalue_key key lam =
  Lapply{
    ap_loc=Loc_unknown;
    ap_func=Lprim(Pfield (toploop_setvalue_pos, Pointer, Mutable),
                  [Lprim(Pgetglobal toploop_ident, [], Loc_unknown)],
                  Loc_unknown);
    ap_args=[Lconst(Const_immstring key); lam];
    ap_tailcall=Default_tailcall;
    ap_inlined=Default_inline;
    ap_specialised=Default_specialise;
  }

let toploop_setvalue id lam = toploop_setvalue_key (toplevel_name id) lam

let toploop_setvalue_id id = toploop_setvalue id (Lvar id)

let close_toplevel_term lam =
  Ident.Set.fold (fun id l -> Llet(Strict, Pgenval, id,
                                  toploop_getvalue id, l))
                (free_variables lam) lam

let transl_toplevel_item ~ctx ~scopes item =
  match item.str_desc with
    Tstr_eval (expr, _)
  | Tstr_value(Nonrecursive, Value,
               [{vb_pat = {pat_desc=Tpat_any};vb_expr = expr}]) ->
      (* special compilation for toplevel "let _ = expr", so
         that Toploop can display the result of the expression.
         Otherwise, the normal compilation would result
         in a Lsequence returning unit. *)
      transl_exp ~ctx ~scopes expr
  | Tstr_value(_, (Macro | Static), _) ->
      (* A phrase with compile-time components is translated as a unit
         is; see [transl_toplevel_phrase]. *)
      Misc.fatal_error "Translmod.transl_toplevel_item"
  | Tstr_value(rec_flag, Value, pat_expr_list) ->
      let idents = let_bound_idents pat_expr_list in
      transl_let ~ctx ~scopes ~in_structure:true rec_flag pat_expr_list
        (make_sequence toploop_setvalue_id idents)
  | Tstr_typext(tyext) ->
      let idents =
        List.map (fun ext -> ext.ext_id) tyext.tyext_constructors
      in
      (* we need to use unique name in case of multiple
         definitions of the same extension constructor in the toplevel *)
      List.iter set_toplevel_unique_name idents;
        transl_type_extension ~ctx ~scopes item.str_env None tyext
          (make_sequence toploop_setvalue_id idents)
  | Tstr_exception ext ->
      set_toplevel_unique_name ext.tyexn_constructor.ext_id;
      toploop_setvalue ext.tyexn_constructor.ext_id
        (transl_extension_constructor ~scopes
           item.str_env None ext.tyexn_constructor)
  | Tstr_module {mb_id=None; mb_presence=Mp_present; mb_expr=modl} ->
      transl_module ~ctx ~scopes Tcoerce_none None modl
  | Tstr_module {mb_id=Some id; mb_presence=Mp_present; mb_expr=modl} ->
      (* we need to use the unique name for the module because of issues
         with "open" (PR#8133) *)
      set_toplevel_unique_name id;
      let lam = transl_module
                  ~ctx ~scopes:(enter_module_definition ~scopes id)
                  Tcoerce_none (Some(Pident id)) modl in
      toploop_setvalue id lam
  | Tstr_recmodule bindings ->
      let idents = List.filter_map (fun mb -> mb.mb_id) bindings in
      compile_recmodule ~scopes
        (fun id modl ->
           match id with
           | None ->
             transl_module ~ctx ~scopes Tcoerce_none None modl
           | Some id ->
             transl_module
               ~ctx ~scopes:(enter_module_definition ~scopes id)
               Tcoerce_none (Some (Pident id)) modl)
        bindings
        (make_sequence toploop_setvalue_id idents)
  | Tstr_class cl_list ->
      (* we need to use unique names for the classes because there might
         be a value named identically *)
      let (ids, class_bindings) = transl_class_bindings ~ctx ~scopes cl_list in
      List.iter set_toplevel_unique_name ids;
      Value_rec_compiler.compile_letrec class_bindings
        (make_sequence toploop_setvalue_id ids)
  | Tstr_include incl ->
      let ids = bound_value_identifiers incl.incl_type in
      let modl = incl.incl_mod in
      let mid = Ident.create_local "include" in
      let rec set_idents pos = function
        [] ->
          lambda_unit
      | id :: ids ->
          Lsequence(toploop_setvalue id
                      (Lprim(Pfield (pos, Pointer, Mutable),
                             [Lvar mid], Loc_unknown)),
                    set_idents (pos + 1) ids) in
      Llet(Strict, Pgenval, mid,
           transl_module ~ctx ~scopes Tcoerce_none None modl, set_idents 0 ids)
  | Tstr_primitive descr ->
      record_primitive descr.val_val;
      lambda_unit
  | Tstr_open od ->
      let pure = pure_module od.open_expr in
      (* this optimization shouldn't be needed because Simplif would
          actually remove the [Llet] when it's not used.
          But since [scan_used_globals] runs before Simplif, we need to do
          it. *)
      begin match od.open_bound_items with
      | [] when pure = Alias -> lambda_unit
      | _ ->
          let ids = bound_value_identifiers od.open_bound_items in
          let mid = Ident.create_local "open" in
          let rec set_idents pos = function
              [] ->
                lambda_unit
            | id :: ids ->
                Lsequence(toploop_setvalue id
                            (Lprim(Pfield (pos, Pointer, Mutable),
                                  [Lvar mid], Loc_unknown)),
                          set_idents (pos + 1) ids)
          in
          Llet(pure, Pgenval, mid,
               transl_module ~ctx ~scopes Tcoerce_none None od.open_expr,
               set_idents 0 ids)
      end
  | Tstr_module ({mb_presence=Mp_absent}) ->
      lambda_unit
  | Tstr_modtype _
  | Tstr_type _
  | Tstr_class_type _
  | Tstr_attribute _ ->
      lambda_unit

let transl_toplevel_item_and_close ~ctx ~scopes itm =
  close_toplevel_term
    (transl_label_init (fun () -> transl_toplevel_item ~ctx ~scopes itm))

(* The identifiers that the compile-time parts of the phrases bind to
   placeholders *)
let toplevel_compile_world_dummies = ref Ident.Set.empty

let transl_toplevel_definition ~ctx str =
  reset_labels ();
  Translprim.clear_used_primitives ();
  (* What the phrase binds has no compile-time value; see
     [transl_toplevel_phrase]. *)
  toplevel_compile_world_dummies :=
    Ident.Set.union !toplevel_compile_world_dummies
      (Ident.Set.of_list (bound_value_identifiers str.str_type));
  make_sequence
    (transl_toplevel_item_and_close ~ctx ~scopes:empty_scopes)
    str.str_items

(* Compile the initialization code for a packed library *)

(* A phrase of the toplevel that has compile-time components or
   top-level splices is translated as a unit is: into its run-time term,
   with a hole for each splice, and a compile-time part that ends by
   building that term.  Where a unit builds a block, the two parts of a
   phrase store what the phrase binds in the value table: the run-time
   value of an identifier under its name, and its compile-time value
   under a second key.  An identifier that an earlier phrase bound is
   read from the table.  One that has no compile-time value is, in the
   compile-time part, the placeholder that it is in the compile-time
   part of a unit. *)

let toplevel_macros_name id = Ident.unique_toplevel_name id ^ "$macros"

let toplevel_splice_failed_key = "*splice-failed*"

(* The identifiers whose compile-time values the table holds *)
let toplevel_compile_time_idents = ref Ident.Set.empty

let toplevel_phrase_has_compile_time_part str =
  Env.get_unit_has_top_level_splice ()
  || exports_macros str
  || Mtype.has_macro_components ~expand_aliases:false str.str_final_env
       (Mty_signature str.str_type)

let transl_toplevel_phrase str =
  reset_labels ();
  Translprim.clear_used_primitives ();
  let scopes = empty_scopes in
  let env = str.str_final_env in
  let ctx = initial_context () in
  ctx.unit_state.compile_world_dummies <- !toplevel_compile_world_dummies;
  (* What the phrase binds, and in which part *)
  let run_time_idents, compile_time_idents =
    List.fold_right
      (fun item (run, compile) ->
         match item with
         | Sig_value (id, vd, _) ->
             begin match vd.val_kind with
             | Val_prim _ -> run, compile
             | _ ->
                 if vd.val_macro = Static then run, id :: compile
                 else if vd.val_staging_level < 0 then begin
                   set_toplevel_unique_name id;
                   id :: run, id :: compile
                 end else id :: run, compile
             end
         | Sig_typext (id, _, _, _) ->
             set_toplevel_unique_name id;
             id :: run, compile
         | Sig_module (id, Mp_present, md, _, _) ->
             set_toplevel_unique_name id;
             id :: run,
             (if Mtype.has_macro_components env md.md_type
              then id :: compile else compile)
         | Sig_class (id, _, _, _) ->
             set_toplevel_unique_name id;
             id :: run, compile
         | Sig_module (_, Mp_absent, _, _, _)
         | Sig_type _ | Sig_modtype _ | Sig_class_type _ -> run, compile)
      str.str_type ([], [])
  in
  (* The code of a splice that raises gives the location of the splice
     to the toplevel. *)
  let report loc code =
    let exn = Ident.create_local "exn" in
    Ltrywith
      (code, exn,
       Lsequence
         (Lapply { ap_func = toploop_getvalue_key toplevel_splice_failed_key;
                   ap_args = [Translquote.const (loc : Location.t)];
                   ap_loc = Loc_unknown; ap_tailcall = Default_tailcall;
                   ap_inlined = Default_inline;
                   ap_specialised = Default_specialise },
          Lprim (Praise Raise_reraise, [Lvar exn], Loc_unknown)))
  in
  let run_term, bind_splice_thunks =
    with_splice_thunks ~ctx ~scopes ~report
      (fun ctx ->
         Translobj.transl_label_init (fun () ->
           match str.str_items with
           | [ { str_desc =
                   ( Tstr_eval (expr, _)
                   | Tstr_value (Nonrecursive, Value,
                                 [ { vb_pat = { pat_desc = Tpat_any };
                                     vb_expr = expr } ]) ) } ] ->
               (* The value of the phrase, which the toplevel prints *)
               transl_exp ~ctx ~scopes expr
           | _ ->
               transl_struct ~ctx ~scopes Loc_unknown [] Tcoerce_none None str
                 ~cont:(fun _ ->
                   make_sequence toploop_setvalue_id run_time_idents)))
  in
  let build = bind_splice_thunks (Translquote.quote_module_lambda run_term) in
  let static =
    let ctx = { ctx with innermost_free_variables = free_variables build } in
    in_world ctx Compile_world @@ fun ctx ->
      transl_struct ~ctx ~scopes Loc_unknown [] Tcoerce_none None str
        ~cont:(fun _ ->
          Lsequence
            (make_sequence
               (fun id ->
                  toploop_setvalue_key (toplevel_macros_name id) (Lvar id))
               compile_time_idents,
             build))
  in
  let static =
    Ident.Set.fold
      (fun id lam ->
         let value =
           if Ident.Set.mem id !toplevel_compile_time_idents
           then toploop_getvalue_key (toplevel_macros_name id)
           else if Ident.Set.mem id ctx.unit_state.compile_world_dummies
           then Lconst const_unit
           else
             Misc.fatal_errorf "Translmod.transl_toplevel_phrase: %a"
               Ident.print id
         in
         Llet (Strict, Pgenval, id, value, lam))
      (free_variables static) static
  in
  List.iter
    (fun id ->
       if not (List.exists (Ident.same id) compile_time_idents)
       then record_compile_world_dummy ctx id)
    run_time_idents;
  toplevel_compile_world_dummies := ctx.unit_state.compile_world_dummies;
  toplevel_compile_time_idents :=
    List.fold_left (fun s id -> Ident.Set.add id s)
      !toplevel_compile_time_idents compile_time_idents;
  static

let get_component = function
    None -> Lconst const_unit
  | Some id -> Lprim(Pgetglobal id, [], Loc_unknown)

let transl_package_flambda ~ctx component_names coercion =
  module_block_size component_names coercion,
  apply_coercion ~ctx Loc_unknown Strict coercion
    (Lprim(Pmakeblock(0, Immutable, None),
           List.map get_component component_names,
           Loc_unknown))

let transl_package ~ctx component_names target_name coercion =
  let components =
    Lprim(Pmakeblock(0, Immutable, None),
          List.map get_component component_names, Loc_unknown) in
  Lprim(Psetglobal target_name,
        [apply_coercion ~ctx Loc_unknown Strict coercion components],
        Loc_unknown)
  (*
  let components =
    match coercion with
      Tcoerce_none ->
        List.map get_component component_names
    | Tcoerce_structure (pos_cc_list, id_pos_list) ->
              (* ignore id_pos_list as the ids are already bound *)
        let g = Array.of_list component_names in
        List.map
          (fun (pos, cc) ->
             apply_coercion ~ctx Strict cc (get_component g.(pos)))
          pos_cc_list
    | _ ->
        assert false in
  Lprim(Psetglobal target_name, [Lprim(Pmakeblock(0, Immutable), components)])
   *)

let transl_store_package ~ctx component_names target_name coercion =
  let rec make_sequence fn pos arg =
    match arg with
      [] -> lambda_unit
    | hd :: tl -> Lsequence(fn pos hd, make_sequence fn (pos + 1) tl) in
  match coercion with
    Tcoerce_none ->
      (List.length component_names,
       make_sequence
         (fun pos id ->
           Lprim(Psetfield(pos, Pointer, Root_initialization),
                 [Lprim(Pgetglobal target_name, [], Loc_unknown);
                  get_component id],
                 Loc_unknown))
         0 component_names)
  | Tcoerce_structure (pos_cc_list, _id_pos_list) ->
      let components =
        Lprim(Pmakeblock(0, Immutable, None),
              List.map get_component component_names,
              Loc_unknown)
      in
      let blk = Ident.create_local "block" in
      (List.length pos_cc_list,
       Llet (Strict, Pgenval, blk,
             apply_coercion ~ctx Loc_unknown Strict coercion components,
             make_sequence
               (fun pos _id ->
                 Lprim(Psetfield(pos, Pointer, Root_initialization),
                       [Lprim(Pgetglobal target_name, [], Loc_unknown);
                        Lprim(Pfield (pos, Pointer, Mutable),
                              [Lvar blk], Loc_unknown)],
                       Loc_unknown))
               0 pos_cc_list))
  (*
              (* ignore id_pos_list as the ids are already bound *)
      let id = Array.of_list component_names in
      (List.length pos_cc_list,
       make_sequence
         (fun dst (src, cc) ->
           Lprim(Psetfield(dst, false),
                 [Lprim(Pgetglobal target_name, []);
                  apply_coercion ~ctx Strict cc (get_component id.(src))]))
         0 pos_cc_list)
  *)
  | _ -> assert false

(* The entry points: each translation starts in a context of its own. *)

let transl_implementation_flambda module_name str_cc =
  transl_implementation_flambda ~ctx:(initial_context ()) module_name str_cc

let transl_implementation module_name str_cc =
  transl_implementation ~ctx:(initial_context ()) module_name str_cc

let transl_store_phrases module_name str =
  transl_store_phrases ~ctx:(initial_context ()) module_name str

let transl_store_implementation module_name str_cc =
  transl_store_implementation ~ctx:(initial_context ()) module_name str_cc

let transl_toplevel_definition str =
  transl_toplevel_definition ~ctx:(initial_context ()) str

let transl_package_flambda component_names coercion =
  transl_package_flambda ~ctx:(initial_context ()) component_names coercion

let transl_package component_names target_name coercion =
  transl_package ~ctx:(initial_context ()) component_names target_name
    coercion

let transl_store_package component_names target_name coercion =
  transl_store_package ~ctx:(initial_context ()) component_names target_name
    coercion

(* Error report *)

open Format_doc
module Style = Misc.Style

let print_cycle ppf cycle =
  let print_ident ppf (x,_) = pp_print_string ppf (Ident.name x) in
  let pp_sep ppf () = fprintf ppf "@ -> " in
  fprintf ppf "%a%a%s"
    (pp_print_list ~pp_sep print_ident) cycle
    pp_sep ()
    (Ident.name @@ fst @@ List.hd cycle)

let rec collect_components = function
  | Pident id -> [Ident.name id]
  | Pdot (p, s) -> collect_components p @ [s]
  | Papply (p, _) -> collect_components p
  | Pextra_ty (p, _) -> collect_components p

let get_relative_path top_module path =
  let comps = collect_components path in
  let comps =
    match comps with
    | h :: (_ :: _ as t) when h = top_module -> t
    | _ -> comps
  in
  String.concat "." comps


let explanation_submsg (id, unsafe_info) =
  match unsafe_info with
  | Unnamed -> assert false (* can't be part of a cycle. *)
  | Unsafe {reason; loc; path} ->
      let print fmt =
        let printer =
          let top_module = Ident.name id in
          let guilty = get_relative_path top_module path in
          doc_printf fmt
            Style.inline_code top_module
            Style.inline_code guilty in
        Location.mkloc printer loc in
      match reason with
      | Unsafe_module_binding ->
          print "Module %a defines an unsafe module, %a ."
      | Unsafe_functor ->
          print "Module %a defines an unsafe functor, %a ."
      | Unsafe_typext ->
          print "Module %a defines an unsafe extension constructor, %a ."
      | Unsafe_non_function ->
          print "Module %a defines an unsafe value, %a ."

let report_error loc = function
  | Circular_dependency cycle ->
      let[@manual.ref "s:recursive-modules"] manual_ref = [ 12; 2 ] in
      Location.errorf ~loc ~sub:(List.map explanation_submsg cycle)
        "Cannot safely evaluate the definition of the following cycle@ \
         of recursively-defined modules:@ %a.@ \
         There are no safe modules in this cycle@ %a."
        print_cycle cycle Misc.print_see_manual manual_ref
  | Conflicting_inline_attributes ->
      Location.errorf "@[Conflicting %a attributes@]"
        Style.inline_code "inline"
  | Static_quotes_local id ->
      Location.errorf ~loc
        "The quotations in this static binding capture %a,@ \
         a run-time name of the enclosing module or functor.@ \
         A static value's quotations may only name run-time values@ \
         of other compilation units (global values).@ \
         To close over local run-time names, use a generator (%a):@ \
         its environment is delivered wherever the generator is called."
        Style.inline_code (Ident.name id)
        Style.inline_code "$let[..]"

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) -> Some (report_error loc err)
      | _ ->
        None
    )

let reset () =
  primitive_declarations := [];
  transl_store_subst := Ident.Map.empty;
  aliased_idents := Ident.empty;
  toplevel_compile_time_idents := Ident.Set.empty;
  toplevel_compile_world_dummies := Ident.Set.empty;
  Env.reset_required_globals ();
  Translprim.clear_used_primitives ()
