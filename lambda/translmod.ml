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
| Template_functor_not_supported
| Template_restriction of string
| Toplevel_macro_module_rebinding
| Toplevel_splice_in_recmodule

exception Error of Location.t * error

module TranslSplices = struct
  let item_splices = ref ([] : (int * lambda) list)

  let reset () =
    item_splices := []

  let static_translate_expr ~scopes exp =
    let open Tast_iterator in
    let translated_separately = ref true in
    let descend_with_flag flag iter node visit =
      let saved = !translated_separately in
      translated_separately := flag;
      visit iter node;
      translated_separately := saved
    in
    let expr_iter iter (e : Typedtree.expression ) =
      let open Tast_iterator in
      match e.exp_desc with
      | Texp_splice { spl_exp; spl_index = Some idx } ->
          Translcore.check_no_objects spl_exp;
          let originals, fresh_vars, code =
            Translquote.transl_close_function Location.none
              (Translquote.fv spl_exp)
              (Translcore.transl_exp ~scopes spl_exp)
          in
          let fn =
            lfunction ~kind:Curried
              ~params:
                (List.map (fun v -> (v, Pgenval)) fresh_vars
                 @ [ Ident.create_local "*unit*", Pgenval ])
              ~return:Pgenval ~body:code ~attr:default_function_attribute
              ~loc:Loc_unknown
          in
          ignore (originals : Ident.t list);
          item_splices := !item_splices @ [(idx, fn)];
      | (Texp_struct_item _ | Texp_pack _) ->
          descend_with_flag false iter e
            Tast_iterator.default_iterator.expr
      | _ -> Tast_iterator.default_iterator.expr iter e
    in
    let module_expr_iter iter (me : Typedtree.module_expr) =
      match me.mod_desc with
      | Tmod_structure _ when !translated_separately -> ()
      | Tmod_functor (Template, _, _) ->
          ()
      | Tmod_functor _ ->
          descend_with_flag false iter me
            Tast_iterator.default_iterator.module_expr
      | _ -> Tast_iterator.default_iterator.module_expr iter me
    in
    let iterator =
      { Tast_iterator.default_iterator with
        expr = expr_iter;
        module_expr = module_expr_iter }
    in
    iterator.structure_item iterator exp

  let transl_item_splices ~scopes expr =
    ignore( static_translate_expr ~scopes expr );

    let splices = !item_splices in
    item_splices := []; splices

end


type world =
  | Run_world
  | Compile_world

let transl_world = ref Run_world

let in_run_world () = !transl_world = Run_world
let in_compile_world () = !transl_world = Compile_world

let in_world w f =
  let saved = !transl_world in
  transl_world := w;
  Fun.protect ~finally:(fun () -> transl_world := saved) f

let at_compile_world f = in_world Compile_world f

let toplevel_mixed_module_path =
  ref ((fun _ _ -> None) : Env.t -> Path.t -> lambda option)

let toplevel_shifted_units = ref Misc.Stdlib.String.Set.empty

let register_toplevel_shifted_units names =
  toplevel_shifted_units :=
    Misc.Stdlib.String.Set.union names !toplevel_shifted_units

let batch_provenance_path env path =
  let path =
    try Env.normalize_module_path None env path
    with Not_found -> path
  in
  let rec root (p : Path.t) =
    match p with
    | Pident id -> if Ident.persistent id then Some id else None
    | Pdot (p, _) | Pextra_ty (p, _) -> root p
    | Papply _ -> None
  in
  match root path with
  | Some id
    when not (Misc.Stdlib.String.Set.mem (Ident.name id)
                !toplevel_shifted_units) -> Some path
  | _ -> None

let transl_module_path loc env path =
  let compile_world =
    in_compile_world () && not (Translcore.in_quotation ())
  in
  let batch_path =
    if not compile_world then None
    else if not (match Env.find_module path env with
                 | md -> Mtype.has_macro_components env md.md_type
                 | exception Not_found -> false)
    then None
    else if not (Translcore.in_toplevel ()) then Some path
    else batch_provenance_path env path
  in
  match batch_path with
  | Some p -> transl_module_macros_path loc env p
  | None ->
      match
        if compile_world && Translcore.in_toplevel ()
        then !toplevel_mixed_module_path env path
        else None
      with
      | Some lam -> lam
      | None -> Lambda.transl_module_path loc env path

let toplevel_splice_hole_idents : (int, Ident.t) Hashtbl.t =
  Hashtbl.create 16

let toplevel_splice_hole_ident idx =
  match Hashtbl.find_opt toplevel_splice_hole_idents idx with
  | Some h -> h
  | None ->
      let h = Ident.create_local "*splice*" in
      Hashtbl.add toplevel_splice_hole_idents idx h;
      h

let insert_item_splices indexed_splices item_lam =
  List.fold_right
    (fun (idx, splice_lam) str_lam ->
       Llet (Strict, Pgenval, toplevel_splice_hole_ident idx,
             splice_lam, str_lam))
    indexed_splices item_lam

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

let transl_type_extension ~scopes env rootpath tyext body =
  List.fold_right
    (fun ext body ->
      let lam =
        transl_extension_constructor ~scopes env
          (field_path rootpath ext.ext_id) ext
      in
      Llet(Strict, Pgenval, ext.ext_id, lam, body))
    tyext.tyext_constructors
    body

(* Compile a coercion *)

let rec apply_coercion loc strict restr arg =
  match restr with
    Tcoerce_none ->
      arg
  | Tcoerce_structure(pos_cc_list, id_pos_list) ->
      name_lambda strict arg (fun id ->
        let get_field pos =
          if pos < 0 then lambda_unit
          else Lprim(Pfield (pos, Pointer, Mutable), [Lvar id], loc)
        in
        let lam =
          Lprim(Pmakeblock(0, Immutable, None),
                List.map (apply_coercion_field loc get_field) pos_cc_list,
                loc)
        in
        wrap_id_pos_list loc id_pos_list get_field lam)
  | Tcoerce_functor(face, cc_arg, cc_res) ->
      (match face with
       | Fcf_mixed when in_compile_world () ->
           apply_coercion loc strict cc_res arg
       | Fcf_template when in_compile_world () ->
           coerce_template_component loc strict cc_arg cc_res arg
       | Fcf_template ->
           arg
       | _ ->
           let param = Ident.create_local "funarg" in
           let carg = apply_coercion loc Alias cc_arg (Lvar param) in
           apply_coercion_result loc strict arg [param, Pgenval] [carg]
             cc_res)
  | Tcoerce_primitive { pc_loc = _; pc_desc; pc_env; pc_type; } ->
      Translprim.transl_primitive loc pc_desc pc_env pc_type None
  | Tcoerce_alias (env, path, cc) ->
      let lam = transl_module_path loc env path in
      name_lambda strict arg
        (fun _ -> apply_coercion loc Alias cc lam)

and apply_coercion_field loc get_field (pos, cc) =
  apply_coercion loc Alias cc (get_field pos)

and coerce_template_component loc strict cc_arg cc_res arg =
  name_lambda strict arg (fun id ->
    let xm = Ident.create_local "*xm*" in
    let pr = Ident.create_local "*tcpair*" in
    let field n = Lprim (Pfield (n, Pointer, Immutable), [Lvar pr], loc) in
    let record = apply_coercion loc Strict cc_res (field 0) in
    let dyn =
      match cc_res with
      | Tcoerce_none | Tcoerce_functor _ -> field 1
      | _ ->
          let envc = Ident.create_local "*envcode*" in
          let frag = Ident.create_local "*frag*" in
          let coerced_frag =
            in_world Run_world @@ fun () ->
            apply_coercion loc Strict cc_res (Lsplice (Lvar frag))
          in
          lfunction ~kind:Curried ~params:[envc, Pgenval] ~return:Pgenval
            ~attr:default_function_attribute ~loc
            ~body:(Llet (Strict, Pgenval, frag,
                     Lapply { ap_func = field 1; ap_args = [Lvar envc];
                              ap_loc = loc;
                              ap_tailcall = Default_tailcall;
                              ap_inlined = Default_inline;
                              ap_specialised = Default_specialise },
                     Translquote.module_builder coerced_frag))
    in
    lfunction ~kind:Curried ~params:[xm, Pgenval] ~return:Pgenval
      ~attr:default_function_attribute ~loc
      ~body:(Llet (Strict, Pgenval, pr,
               Lapply { ap_func = Lvar id;
                        ap_args =
                          [ apply_coercion loc Strict cc_arg (Lvar xm) ];
                        ap_loc = loc; ap_tailcall = Default_tailcall;
                        ap_inlined = Default_inline;
                        ap_specialised = Default_specialise },
               Lprim (Pmakeblock (0, Immutable, None), [record; dyn],
                      loc))))

and apply_coercion_result loc strict funct params args cc_res =
  match cc_res with
  | Tcoerce_functor(_, cc_arg, cc_res) ->
    let param = Ident.create_local "funarg" in
    let arg = apply_coercion loc Alias cc_arg (Lvar param) in
    apply_coercion_result loc strict funct
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
             ~body:(apply_coercion
                   loc Strict cc_res
                   (Lapply{
                      ap_loc=loc;
                      ap_func=Lvar id;
                      ap_args=List.rev args;
                      ap_tailcall=Default_tailcall;
                      ap_inlined=Default_inline;
                      ap_specialised=Default_specialise;
                    })))

and wrap_id_pos_list loc id_pos_list get_field lam =
  let fv = free_variables lam in
  (*Format.eprintf "%a@." Printlambda.lambda lam;
  Ident.Set.iter (fun id -> Format.eprintf "%a " Ident.print id) fv;
  Format.eprintf "@.";*)
  let (lam, _fv, s) =
    List.fold_left (fun (lam, fv, s) (id',pos,c) ->
      if Ident.Set.mem id' fv then
        let id'' = Ident.create_local (Ident.name id') in
        let rhs = apply_coercion loc Alias c (get_field pos) in
        let fv_rhs = free_variables rhs in
        (Llet(Alias, Pgenval, id'', rhs, lam),
         Ident.Set.union fv fv_rhs,
         Ident.Map.add id' id'' s)
      else (lam, fv, s))
      (lam, fv, Ident.Map.empty) id_pos_list
  in
  if s == Ident.Map.empty then lam else Lambda.rename s lam


(* Compose two coercions
   apply_coercion c1 (apply_coercion c2 e) behaves like
   apply_coercion (compose_coercions c1 c2) e. *)

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
  | (Tcoerce_functor(face, arg1, res1), Tcoerce_functor(_, arg2, res2)) ->
      Tcoerce_functor(face, compose_coercions arg2 arg1,
                      compose_coercions res1 res2)
  | (c1, Tcoerce_alias (env, path, c2)) ->
      Tcoerce_alias (env, path, compose_coercions c1 c2)
  | (_, _) ->
      fatal_error "Translmod.compose_coercions"

(*
let apply_coercion a b c =
  Format.eprintf "@[<2>apply_coercion@ %a@]@." Includemod.print_coercion b;
  apply_coercion a b c

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

let compile_recmodule ~scopes compile_rhs bindings cont =
  eval_rec_bindings
    (reorder_rec_bindings
       (List.map
          (fun {mb_id=id; mb_name; mb_expr=modl; _} ->
             let id_or_ignore_loc, shape =
               match id with
               | None ->
                 let loc = of_location ~scopes mb_name.loc in
                 Ignore_loc loc, Result.Error Unnamed
               | Some id -> Id id, init_shape id modl
             in
             (id_or_ignore_loc, modl.mod_loc, shape, compile_rhs id modl))
          bindings))
    cont

(* Code to translate class entries in a structure *)

let transl_class_bindings ~scopes cl_list =
  let ids = List.map (fun (ci, _) -> ci.ci_id_class) cl_list in
  (ids,
   List.map
     (fun ({ci_id_class=id; ci_expr=cl; ci_virt=vf}, meths) ->
       let def, rkind = transl_class ~scopes ids id meths cl vf in
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
    | Tmod_functor (_, param, body) ->
      let inline_attribute' =
        Translattribute.get_inline_attribute mexp.mod_attributes
      in
      let arg_coercion, res_coercion =
        match coercion with
        | Tcoerce_none -> Tcoerce_none, Tcoerce_none
        | Tcoerce_functor (_, arg_coercion, res_coercion) ->
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

let rec address_root = function
  | Env.Aident id -> id
  | Env.Adot (a, _) -> address_root a

let expr_macro_call_env_roots e0 =
  let roots = ref Ident.Set.empty in
  let expr iter (e : Typedtree.expression) =
    (match e.exp_desc with
     | Texp_ident (path, _, desc) when Types.val_is_macro desc ->
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

let macro_captured_root_set vb =
  Ident.Set.union
    (Translquote.fv vb.vb_expr)
    (macro_call_env_roots vb)
  |> Ident.Set.filter (fun h -> not (Ident.global h))

let macro_captured_roots vb =
  Ident.Set.elements (macro_captured_root_set vb)

let macro_bound_id vb =
  match let_bound_idents [vb] with
  | [id] -> id
  | _ -> Misc.fatal_error "Translmod: a macro binds other than one name"

let macro_group_captured_roots pat_expr_list =
  let members =
    List.fold_left (fun s vb -> Ident.Set.add (macro_bound_id vb) s)
      Ident.Set.empty pat_expr_list
  in
  List.fold_left
    (fun s vb -> Ident.Set.union s (macro_captured_root_set vb))
    Ident.Set.empty pat_expr_list
  |> (fun s -> Ident.Set.diff s members)
  |> Ident.Set.elements

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

let macro_env_of_roots = function
  | [] -> Lconst const_unit
  | roots ->
      Lprim (Pmakeblock (0, Immutable, None),
             List.map (fun h -> Lvar h) roots, Loc_unknown)

let macro_env_slots rec_flag pat_expr_list body =
  let env_of_roots = macro_env_of_roots in
  match rec_flag with
  | Nonrecursive ->
      List.fold_right
        (fun vb body ->
           Llet (Strict, Pgenval, macro_bound_id vb,
                 env_of_roots (macro_captured_roots vb), body))
        pat_expr_list body
  | Recursive ->
      match macro_group_captured_roots pat_expr_list with
      | [] ->
          List.fold_right
            (fun vb body ->
               Llet (Strict, Pgenval, macro_bound_id vb,
                     Lconst const_unit, body))
            pat_expr_list body
      | roots ->
          match pat_expr_list with
          | [] -> assert false
          | first_vb :: rest ->
              let first = macro_bound_id first_vb in
              Llet (Strict, Pgenval, first, env_of_roots roots,
                    List.fold_right
                      (fun vb body ->
                         Llet (Alias, Pgenval, macro_bound_id vb,
                               Lvar first, body))
                      rest body)

let transl_macro_group_functions ?fns ~scopes rec_flag pat_expr_list =
  let group =
    List.fold_left (fun s vb -> Ident.Set.add (macro_bound_id vb) s)
      Ident.Set.empty pat_expr_list
  in
  let group_index =
    match rec_flag with
    | Recursive ->
        Some (List.mapi (fun i h -> (h, [i]))
                (macro_group_captured_roots pat_expr_list))
    | Nonrecursive -> None
  in
  List.iter (fun vb -> Translcore.check_no_objects vb.vb_expr)
    pat_expr_list;
  Translcore.with_defining_macros ?fns group @@ fun () ->
  List.map (fun vb ->
    let id = macro_bound_id vb in
    let me_index =
      match group_index with
      | Some index -> index
      | None ->
          List.mapi (fun i h -> (h, [i])) (macro_captured_roots vb)
    in
    let me_param = Ident.create_local "macro_env" in
    let fn =
      Translcore.with_macro_env
        (Some { Translcore.me_param; me_index })
        (fun () -> transl_exp ~scopes vb.vb_expr)
    in
    (id, vb, add_macro_env_param me_param (Translquote.remove_events fn)))
    pat_expr_list

let toplevel_macro_pairs ~scopes rec_flag pat_expr_list k =
  let fn_alias =
    List.fold_left
      (fun m vb ->
         let id = macro_bound_id vb in
         Ident.Map.add id (Ident.create_local (Ident.name id ^ "$fn")) m)
      Ident.Map.empty pat_expr_list
  in
  let fns =
    transl_macro_group_functions ~fns:fn_alias ~scopes rec_flag
      pat_expr_list
  in
  let pair id env =
    Lprim (Pmakeblock (0, Immutable, None),
           [Lvar (Ident.Map.find id fn_alias); env], Loc_unknown)
  in
  let bind_fns body =
    match rec_flag with
    | Nonrecursive ->
        List.fold_right
          (fun (id, _, fn) body ->
             Llet (Strict, Pgenval, Ident.Map.find id fn_alias, fn, body))
          fns body
    | Recursive ->
        let rec_binding (id, _, fn) =
          match fn with
          | Lfunction def -> { id = Ident.Map.find id fn_alias; def }
          | _ -> Misc.fatal_error "Translmod: a macro is not a function"
        in
        Lletrec (List.map rec_binding fns, body)
  in
  bind_fns
    (if in_compile_world () then
       k (List.map (fun (id, _, _) -> (id, pair id (Lconst const_unit)))
            fns)
     else
       match rec_flag with
       | Nonrecursive ->
           k (List.map (fun (id, vb, _) ->
                  (id,
                   pair id (macro_env_of_roots (macro_captured_roots vb))))
                fns)
       | Recursive ->
           let shared = Ident.create_local "*macro_group_env*" in
           Llet (Strict, Pgenval, shared,
                 macro_env_of_roots
                   (macro_group_captured_roots pat_expr_list),
                 k (List.map (fun (id, _, _) -> (id, pair id (Lvar shared)))
                      fns)))

let template_functor_parts me =
  match me.Typedtree.mod_desc with
  | Tmod_functor (Template, param, body) -> Some (param, body)
  | _ -> None

let template_bound_idents (body : Typedtree.module_expr) =
  let bound = ref Ident.Set.empty in
  let add x = bound := Ident.Set.add x !bound in
  let pat : type k . _ -> k Typedtree.general_pattern -> unit =
    fun iter p ->
      List.iter add (Typedtree.pat_bound_idents p);
      Tast_iterator.default_iterator.pat iter p
  in
  let expr iter (e : Typedtree.expression) =
    (match e.exp_desc with
     | Texp_for (x, _, _, _, _, _) -> add x
     | Texp_letop { param; _ } -> add param
     | Texp_function (params, fbody) ->
         List.iter (fun fp -> add fp.Typedtree.fp_param) params;
         (match fbody with
          | Tfunction_cases { param; _ } -> add param
          | Tfunction_body _ -> ())
     | _ -> ());
    Tast_iterator.default_iterator.expr iter e
  in
  let structure_item iter (it : Typedtree.structure_item) =
    (match it.str_desc with
     | Tstr_module { mb_id = Some id; _ } -> add id
     | Tstr_recmodule mbs ->
         List.iter (fun mb -> Option.iter add mb.Typedtree.mb_id) mbs
     | Tstr_exception ext -> add ext.tyexn_constructor.ext_id
     | Tstr_typext tyext ->
         List.iter (fun ext -> add ext.Typedtree.ext_id)
           tyext.tyext_constructors
     | Tstr_include incl ->
         List.iter add (bound_value_identifiers incl.incl_type)
     | Tstr_open od ->
         List.iter add (bound_value_identifiers od.open_bound_items)
     | _ -> ());
    Tast_iterator.default_iterator.structure_item iter it
  in
  let module_expr iter (me : Typedtree.module_expr) =
    (match me.mod_desc with
     | Tmod_functor (_, Named (Some x, _, _), _) -> add x
     | _ -> ());
    Tast_iterator.default_iterator.module_expr iter me
  in
  let iter =
    { Tast_iterator.default_iterator with pat; expr; structure_item;
      module_expr }
  in
  iter.module_expr iter body;
  !bound

let check_template_body (body : Typedtree.module_expr) =
  let expr _iter (e : Typedtree.expression) =
    Translcore.check_no_objects e
  in
  let structure_item iter (it : Typedtree.structure_item) =
    (match it.str_desc with
     | Tstr_class _ ->
         raise (Error (it.str_loc, Template_restriction
           "Classes are not yet supported in a template functor body."))
     | _ -> ());
    Tast_iterator.default_iterator.structure_item iter it
  in
  let iter =
    { Tast_iterator.default_iterator with expr; structure_item }
  in
  iter.module_expr iter body

type template_translation =
  { tt_roots : Ident.t list;
    tt_function : lambda }

type template_application_idents =
  { ta_mcode : Ident.t;    ta_arg : Ident.t;
    ta_funct : Ident.t }

let template_applications =
  ref (Ident.Map.empty : template_application_idents Ident.Map.t)

let template_include_binders = ref ([] : (Location.t * Ident.t) list)

let template_include_binder loc =
  match List.assoc_opt loc !template_include_binders with
  | Some id -> id
  | None ->
      let id = Ident.create_local "*tincl*" in
      template_include_binders := (loc, id) :: !template_include_binders;
      id

let reset_template_applications () =
  template_applications := Ident.Map.empty;
  template_include_binders := []

let template_application_idents m_id =
  match Ident.Map.find_opt m_id !template_applications with
  | Some p -> p
  | None ->
      let p = { ta_mcode = Ident.create_local "*tapp*";
                ta_arg = Ident.create_local "*targ*";
                ta_funct = Ident.create_local "*tfunct*" } in
      template_applications := Ident.Map.add m_id p !template_applications;
      p

let template_application_slot m_id = (template_application_idents m_id).ta_mcode

type template_apps_mode =
  | Tapps_inactive
  | Tapps_items
  | Tapps_plain_body  | Tapps_template_body
  | Tapps_template_plain_body

let template_apps_mode = ref Tapps_inactive

let with_template_apps m f =
  let saved = !template_apps_mode in
  template_apps_mode := m;
  Fun.protect ~finally:(fun () -> template_apps_mode := saved) f

let batch_structure_context () =
  not (Translcore.in_toplevel ())
  || !template_apps_mode = Tapps_template_body
  || !template_apps_mode = Tapps_template_plain_body
  || Translcore.current_template_batch_idents () <> None

let template_plain_body_locals = ref Ident.Set.empty

let with_plain_body_locals s f =
  let saved = !template_plain_body_locals in
  template_plain_body_locals := s;
  Fun.protect ~finally:(fun () -> template_plain_body_locals := saved) f

let sig_module_idents acc sg =
  List.fold_left
    (fun acc item ->
       match item with
       | Types.Sig_module (id, _, _, _, _) -> Ident.Set.add id acc
       | _ -> acc)
    acc sg

let rec bound_module_idents_of_module acc me =
  match me.Typedtree.mod_desc with
  | Tmod_structure str -> bound_module_idents_of_structure acc str
  | Tmod_constraint (m, _, _, _) -> bound_module_idents_of_module acc m
  | Tmod_functor (_, _, body) -> bound_module_idents_of_module acc body
  | _ -> acc

and bound_module_idents_of_structure acc str =
  List.fold_left
    (fun acc item ->
       match item.Typedtree.str_desc with
       | Tstr_module mb ->
           let acc = match mb.mb_id with
             | Some id -> Ident.Set.add id acc
             | None -> acc
           in
           bound_module_idents_of_module acc mb.mb_expr
       | Tstr_recmodule mbs ->
           List.fold_left
             (fun acc mb ->
                match mb.Typedtree.mb_id with
                | Some id -> Ident.Set.add id acc
                | None -> acc)
             acc mbs
       | Tstr_include incl ->
           bound_module_idents_of_module
             (sig_module_idents acc incl.incl_type) incl.incl_mod
       | Tstr_open od -> sig_module_idents acc od.open_bound_items
       | _ -> acc)
    acc str.str_items

let translating_macros_object = ref false

type template_body_app =
  { tba_m_id : Ident.t;
    tba_hole : Ident.t;
    tba_pair : Ident.t;
    tba_pair_term : lambda;
    tba_lit_funct : Ident.t option;
    tba_lit_arg : Ident.t option }

let template_body_apps =
  ref (None : template_body_app list ref option)

let with_template_body_apps r f =
  let saved = !template_body_apps in
  template_body_apps := r;
  Fun.protect ~finally:(fun () -> template_body_apps := saved) f

let template_body_pending_chain =
  ref (None : (lambda -> lambda) list ref option)

let with_template_body_pending_chain r f =
  let saved = !template_body_pending_chain in
  template_body_pending_chain := r;
  Fun.protect ~finally:(fun () -> template_body_pending_chain := saved) f

let rec template_application_parts me =
  match me.Typedtree.mod_desc with
  | Tmod_apply (Template, funct, arg, ccarg) -> Some (funct, Some (arg, ccarg))
  | Tmod_apply_unit (Template, funct) -> Some (funct, None)
  | Tmod_constraint (inner, _, _, _) -> template_application_parts inner
  | _ -> None

let rec template_application_rescc me =
  match me.Typedtree.mod_desc with
  | Tmod_constraint (inner, _, _, cc) ->
      compose_coercions cc (template_application_rescc inner)
  | _ -> Tcoerce_none

let rec module_expr_path me =
  match me.Typedtree.mod_desc with
  | Tmod_ident (p, _) -> Some p
  | Tmod_constraint (m, _, _, _) -> module_expr_path m
  | _ -> None

let is_module_path me = module_expr_path me <> None

let module_has_template_definition (me : Typedtree.module_expr) =
  let found = ref false in
  let structure_item iter (it : Typedtree.structure_item) =
    (match it.str_desc with
     | Tstr_module { mb_expr; _ }
       when template_functor_parts mb_expr <> None -> found := true
     | _ -> ());
    Tast_iterator.default_iterator.structure_item iter it
  in
  let iter = { Tast_iterator.default_iterator with structure_item } in
  iter.module_expr iter me;
  !found

let is_mixed_module_type env mty = Mtype.has_macro_components env mty

let with_mixed_body_batch_idents (mexp : Typedtree.module_expr) f =
  if not (Translcore.in_toplevel ()) then f ()
  else begin
    let rec peel acc (me : Typedtree.module_expr) =
      match me.mod_desc with
      | Tmod_functor (_, Named (Some id, _, _), body) ->
          peel (Ident.Set.add id acc) body
      | Tmod_functor (_, _, body) -> peel acc body
      | Tmod_constraint (me', _, _, _) -> peel acc me'
      | _ -> (acc, me)
    in
    let params, core = peel Ident.Set.empty mexp in
    let s = Ident.Set.union params (template_bound_idents core) in
    let s =
      match Translcore.current_template_batch_idents () with
      | Some s0 -> Ident.Set.union s0 s
      | None -> s
    in
    Translcore.with_template_batch_idents (Some s) f
  end

let functor_result_coercion = function
  | Tcoerce_functor (_, _, cc_res) -> cc_res
  | cc -> cc

let rec peel_functor_coercions = function
  | Tcoerce_functor (_, _, cc_res) -> peel_functor_coercions cc_res
  | cc -> cc

let rec is_plain_functor me =
  match me.Typedtree.mod_desc with
  | Tmod_functor (Plain, _, _) -> true
  | Tmod_constraint (m, _, _, _) -> is_plain_functor m
  | _ -> false

let rec has_template_applications str =
  List.exists
    (fun item -> match item.Typedtree.str_desc with
       | Tstr_module mb -> module_has_template_application mb.mb_expr
       | Tstr_recmodule mbs ->
           List.exists (fun mb -> module_has_template_application mb.mb_expr)
             mbs
       | Tstr_include incl -> module_has_template_application incl.incl_mod
       | Tstr_open od -> module_has_template_application od.open_expr
       | _ -> false)
    str.str_items

and module_has_template_application mexp =
  match mexp.Typedtree.mod_desc with
  | Tmod_apply (Template, _, _, _) | Tmod_apply_unit (Template, _) -> true
  | Tmod_structure str -> has_template_applications str
  | Tmod_constraint (m, _, _, _) -> module_has_template_application m
  | Tmod_functor (_, _, body) -> module_has_template_application body
  | _ -> false

let peel_template_params me =
  let param_id p =
    match p with
    | Typedtree.Named (Some x, _, _) -> x
    | Named (None, _, _) | Unit -> Ident.create_local "*X*"
  in
  let rec peel acc me =
    match template_functor_parts me with
    | Some (p, fbody) -> peel (param_id p :: acc) fbody
    | None -> (List.rev acc, me)
  in
  peel [] me

let literal_argument_compile_parts (arg : Typedtree.module_expr) =
  let found = ref false in
  let expr iter (e : Typedtree.expression) =
    (match e.exp_desc with
     | Texp_splice { spl_index = Some _; _ } -> found := true
     | _ -> ());
    if not !found then Tast_iterator.default_iterator.expr iter e
  in
  let module_expr iter (me : Typedtree.module_expr) =
    (match me.mod_desc with
     | Tmod_apply (Template, _, _, _) | Tmod_apply_unit (Template, _) ->
         found := true
     | _ -> ());
    if not !found then Tast_iterator.default_iterator.module_expr iter me
  in
  let iter = { Tast_iterator.default_iterator with expr; module_expr } in
  iter.module_expr iter arg;
  !found

let template_plain_body_path_checks ~locals ~loc funct =
  let check_head what me =
    match module_expr_path me with
    | Some p when Ident.Set.mem (Path.head p) locals ->
        raise (Error (loc, Template_restriction
          (what ^ " must not be rooted at a module defined inside \
                   the enclosing functor body.")))
    | _ -> ()
  in
  check_head "The applied template functor" funct

let template_application_shape_checks ~loc funct =
  if not (is_module_path funct)
     && template_functor_parts funct = None then
    raise (Error (loc, Template_restriction
      "The applied template functor must be a path or a literal \
       template functor."));
  if !template_apps_mode = Tapps_plain_body
     || !template_apps_mode = Tapps_template_plain_body then
    template_plain_body_path_checks ~locals:!template_plain_body_locals
      ~loc funct;
  ()

let anonymous_application_ids : (Location.t, Ident.t) Hashtbl.t =
  Hashtbl.create 8
let anonymous_application_id loc =
  match Hashtbl.find_opt anonymous_application_ids loc with
  | Some id -> id
  | None ->
      let id = Ident.create_local "*anonymous-application*" in
      Hashtbl.add anonymous_application_ids loc id;
      id

let template_application_shape mb =
  let funct, argopt =
    Option.get (template_application_parts mb.Typedtree.mb_expr)
  in
  let m_id =
    match mb.mb_id with
    | Some id -> id
    | None -> anonymous_application_id mb.Typedtree.mb_loc
  in
  template_application_shape_checks ~loc:mb.Typedtree.mb_loc funct;
  (m_id, funct, argopt)

let template_application_mode_check ~loc =
  match !template_apps_mode with
  | Tapps_items | Tapps_plain_body -> ()
  | Tapps_template_body | Tapps_template_plain_body ->
      raise (Error (loc, Template_restriction
        "A template functor cannot be applied at this point inside \
         another template functor's body."))
  | Tapps_inactive ->
      if not !translating_macros_object then
        raise (Error (loc, Template_restriction
          "A template functor can only be applied where the compiler \
           evaluates compile-time code: at the top level of a module, or \
           inside another template functor's body."))

let template_application_checks mb =
  template_application_mode_check ~loc:mb.Typedtree.mb_loc;
  template_application_shape mb

type plain_body_template_application =
  { pba_loc : Location.t;
    pba_m_id : Ident.t;
    pba_funct : Typedtree.module_expr;
    pba_argopt : (Typedtree.module_expr * Typedtree.module_coercion) option }

let plain_body_template_applications me =
  let apps = ref [] in
  let add ~loc m_id funct argopt =
    apps := { pba_loc = loc; pba_m_id = m_id;
              pba_funct = funct; pba_argopt = argopt } :: !apps
  in
  let rec scan_module me =
    match me.Typedtree.mod_desc with
    | Tmod_structure str -> List.iter scan_item str.str_items
    | Tmod_constraint (m, _, _, _) -> scan_module m
    | Tmod_functor (Plain, _, body) -> scan_module body
    | _ -> ()
  and scan_item item =
    match item.Typedtree.str_desc with
    | Tstr_module mb when template_application_parts mb.mb_expr <> None ->
        let m_id, funct, argopt =
          template_application_shape mb
        in
        add ~loc:mb.mb_loc m_id funct argopt
    | Tstr_module mb -> scan_module mb.mb_expr
    | Tstr_include incl ->
        (match template_application_parts incl.incl_mod with
         | Some (funct, argopt) ->
             let loc = incl.incl_loc in
             template_application_shape_checks ~loc funct;
             add ~loc (template_include_binder loc) funct argopt
         | None ->
             scan_module incl.incl_mod)
    | Tstr_open od ->
        scan_module od.open_expr
    | _ -> ()
  in
  scan_module me;
  List.rev !apps

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

let dummy_slots ids next fields =
  let body = next (List.rev_append ids fields) in
  List.fold_right
    (fun id body -> Llet (Alias, Pgenval, id, Lconst const_unit, body))
    ids body

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

let close_toplevel_term ?(except = Ident.Set.empty) lam =
  Ident.Set.fold (fun id l -> Llet(Strict, Pgenval, id,
                                  toploop_getvalue id, l))
                (Ident.Set.diff (free_variables lam) except) lam

let toplevel_subst_table_reads lam =
  let free =
    Ident.Set.filter (fun id -> not (Ident.global id))
      (free_variables lam)
  in
  if Ident.Set.is_empty free then lam
  else
    Lambda.subst (fun _ _ env -> env)
      (Ident.Set.fold
         (fun id m -> Ident.Map.add id (toploop_getvalue id) m)
         free Ident.Map.empty)
      lam

let toplevel_template_functors = ref Ident.Set.empty

let register_toplevel_template_functor id =
  toplevel_template_functors :=
    Ident.Set.add id !toplevel_template_functors

let template_component_key id = toplevel_name id ^ "$template"
let toplevel_record_key id = toplevel_name id ^ "$record"

let static_unit_suffix = "$static"

let () =
  Lambda.persistent_address_hook :=
    (fun id ->
       if Translcore.in_toplevel ()
          && (in_compile_world () || Translcore.in_macro_body ())
          && not (Translcore.in_quotation ())
          && Misc.Stdlib.String.Set.mem (Ident.name id)
               !toplevel_shifted_units
       then Ident.create_persistent (Ident.name id ^ static_unit_suffix)
       else id)

let toplevel_mixed_key id = toplevel_name id ^ "$mixed"

let toplevel_mixed_functors = ref Ident.Set.empty

let register_toplevel_mixed_functor id =
  toplevel_mixed_functors := Ident.Set.add id !toplevel_mixed_functors

let () =
  toplevel_mixed_module_path :=
    (fun env path ->
       let path =
         try Env.normalize_module_path None env path
         with Not_found -> path
       in
       match (path : Path.t) with
       | Pident id when Ident.Set.mem id !toplevel_mixed_functors ->
           Some (toploop_getvalue_key (toplevel_mixed_key id))
       | _ ->
           let rec root (p : Path.t) =
             match p with
             | Pident id -> Some id
             | Pdot (p, _) | Pextra_ty (p, _) -> root p
             | Papply _ -> None
           in
           match root path with
           | Some id when Translcore.is_toplevel_template_module id ->
               (match Env.find_module_address path env with
                | addr ->
                    let rec reroot base = function
                      | Env.Aident _ -> base
                      | Env.Adot (a, pos) ->
                          Lprim (Pfield (pos, Pointer, Immutable),
                                 [reroot base a], Loc_unknown)
                    in
                    Some
                      (reroot
                         (toploop_getvalue_key (toplevel_record_key id))
                         addr)
                | exception Not_found -> None)
           | _ -> None)

let prim_obj_dup =
  Pccall (Primitive.simple ~name:"caml_obj_dup" ~arity:1 ~alloc:true)

let toplevel_adapted_view ~pair_half (arg : Typedtree.module_expr)
      root_lam =
  match module_expr_path arg with
  | None -> root_lam
  | Some path ->
      let env = arg.mod_env in
      let last_pos = function
        | Env.Adot (_, pos) -> Some pos
        | Env.Aident _ -> None
      in
      let rec view path mty root_lam =
        match Mtype.scrape env mty with
        | Types.Mty_signature sg ->
            let r = Ident.create_local "*targ*" in
            let c = Ident.create_local "*tview*" in
            let field pos lam =
              Lprim (Pfield (pos, Pointer, Mutable), [lam], Loc_unknown)
            in
            let sets =
              List.filter_map
                (fun item ->
                   match (item : Types.signature_item) with
                   | Sig_value (id, vd, _)
                     when vd.val_staging_level < 0
                          && (match vd.val_kind with
                              | Val_prim _ -> false
                              | _ -> true) ->
                       (match
                          Env.find_value_address
                            (Pdot (path, Ident.name id)) env
                        with
                        | addr ->
                            Option.map
                              (fun pos ->
                                 (pos,
                                  Lprim (Pfield (pair_half, Pointer,
                                                 Immutable),
                                         [field pos (Lvar r)],
                                         Loc_unknown)))
                              (last_pos addr)
                        | exception Not_found -> None)
                   | Sig_module (id, Mp_present, md, _, _)
                     when Mtype.has_macro_components env md.md_type ->
                       (match
                          Env.find_module_address
                            (Pdot (path, Ident.name id)) env
                        with
                        | addr ->
                            Option.map
                              (fun pos ->
                                 (pos,
                                  view (Pdot (path, Ident.name id))
                                    md.md_type (field pos (Lvar r))))
                              (last_pos addr)
                        | exception Not_found -> None)
                   | _ -> None)
                sg
            in
            Llet (Strict, Pgenval, r, root_lam,
              Llet (Strict, Pgenval, c,
                    Lprim (prim_obj_dup, [Lvar r], Loc_unknown),
                List.fold_right
                  (fun (pos, lam) acc ->
                     Lsequence
                       (Lprim (Psetfield (pos, Pointer, Assignment),
                               [Lvar c; lam], Loc_unknown),
                        acc))
                  sets (Lvar c)))
        | _ -> root_lam
      in
      view path arg.mod_type root_lam

let toplevel_compile_time_view arg root_lam =
  toplevel_adapted_view ~pair_half:0 arg root_lam

let toplevel_run_time_view arg root_lam =
  toplevel_adapted_view ~pair_half:1 arg root_lam

let toplevel_run_time_argument (arg : Typedtree.module_expr) lam =
  match module_expr_path arg with
  | None -> lam
  | Some path ->
      let head =
        match
          Env.normalize_module_path None arg.mod_env path
        with
        | path -> Path.head path
        | exception Not_found -> Path.head path
      in
      if Ident.global head
         || Translcore.in_template_batch_set head
         || Translcore.is_toplevel_template_module head
      then lam
      else toplevel_run_time_view arg lam

let rec compile_functor ~scopes mexp coercion root_path loc =
  let functor_params_rev, body, body_path, res_coercion, inline_attribute =
    merge_functors ~scopes mexp coercion root_path
  in
  assert (List.length functor_params_rev >= 1);  (* cf. [transl_module] *)
  let params, body =
    List.fold_left (fun (params, body) (param, loc, arg_coercion) ->
        let param' = Ident.rename param in
        let arg = apply_coercion loc Alias arg_coercion (Lvar param') in
        let params = (param', Pgenval) :: params in
        let body = Llet (Alias, Pgenval, param, arg, body) in
        params, body)
      ([], transl_module ~scopes res_coercion body_path body)
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

and transl_module ~scopes cc rootpath mexp =
  let loc = of_location ~scopes mexp.mod_loc in
  match mexp.mod_desc with
  | Tmod_ident (path,_) ->
      let cc =
        match cc with
        | Tcoerce_functor (Fcf_mixed, _, _) when in_compile_world () ->
            peel_functor_coercions cc
        | _ -> cc
      in
      apply_coercion loc Strict cc
        (transl_module_path loc mexp.mod_env path)
  | Tmod_structure str ->
      transl_struct ~scopes loc [] cc rootpath str
  | Tmod_functor (Template, _, _) ->
      raise (Error (mexp.mod_loc, Template_functor_not_supported))
  | Tmod_apply (Template, _, _, _)
  | Tmod_apply_unit (Template, _) ->
      if Translcore.in_toplevel () then
        raise (Error (mexp.mod_loc, Template_functor_not_supported))
      else
        raise (Error (mexp.mod_loc, Template_restriction
          "A template application is supported only as the entire \
           right-hand side of a module binding (module M = F[V]) \
           or of an include (include F[V])."))
  | Tmod_functor (Plain, _, fbody)
    when in_compile_world ()
         && is_mixed_module_type mexp.mod_env mexp.mod_type ->
      with_mixed_body_batch_idents mexp @@ fun () ->
      with_template_apps Tapps_plain_body @@ fun () ->
      transl_module ~scopes (functor_result_coercion cc) rootpath fbody
  | Tmod_functor _ ->
      let mode =
        match !template_apps_mode with
        | Tapps_items | Tapps_plain_body -> Tapps_plain_body
        | Tapps_template_body | Tapps_template_plain_body ->
            Tapps_template_plain_body
        | Tapps_inactive as m -> m
      in
      let locals =
        if (mode = Tapps_plain_body || mode = Tapps_template_plain_body)
           && module_has_template_application mexp
        then
          bound_module_idents_of_module !template_plain_body_locals mexp
        else !template_plain_body_locals
      in
      with_template_apps mode @@ fun () ->
      with_plain_body_locals locals @@ fun () ->
      (if mode = Tapps_template_plain_body then (fun f -> f ())
       else with_template_body_apps None) @@ fun () ->
      (if is_mixed_module_type mexp.mod_env mexp.mod_type
       then with_mixed_body_batch_idents mexp
       else (fun f -> f ())) @@ fun () ->
      oo_wrap mexp.mod_env true (fun () ->
        compile_functor ~scopes mexp cc rootpath loc) ()
  | Tmod_apply(_, funct, arg, ccarg) ->
      if in_compile_world ()
         && is_mixed_module_type funct.mod_env funct.mod_type then
        apply_coercion loc Strict cc
          (transl_module ~scopes Tcoerce_none None funct)
      else
      let translated_arg = transl_module ~scopes ccarg None arg in
      transl_apply ~scopes ~loc ~cc mexp.mod_env funct translated_arg
  | Tmod_apply_unit (_, funct) ->
      if in_compile_world ()
         && is_mixed_module_type funct.mod_env funct.mod_type then
        apply_coercion loc Strict cc
          (transl_module ~scopes Tcoerce_none None funct)
      else
      transl_apply ~scopes ~loc ~cc mexp.mod_env funct lambda_unit
  | Tmod_constraint(arg, _, _, ccarg) ->
      transl_module ~scopes (compose_coercions cc ccarg) rootpath arg
  | Tmod_unpack(arg, _) ->
      apply_coercion loc Strict cc (Translcore.transl_exp ~scopes arg)

and transl_apply ~scopes ~loc ~cc mod_env funct translated_arg =
  let inlined_attribute =
    Translattribute.get_inlined_attribute_on_module funct
  in
  oo_wrap mod_env true
    (apply_coercion loc Strict cc)
    (Lapply{
       ap_loc=loc;
       ap_func=transl_module ~scopes Tcoerce_none None funct;
       ap_args=[translated_arg];
       ap_tailcall=Default_tailcall;
       ap_inlined=inlined_attribute;
       ap_specialised=Default_specialise})

and transl_struct ?cont ~scopes loc fields cc rootpath
      {str_final_env; str_items; _} =
  transl_structure ?cont ~scopes loc fields cc rootpath str_final_env
    str_items

(* The function  transl_structure is called by  the bytecode compiler.
   Some effort is made to compile in top to bottom order, in order to display
   warning by increasing locations. *)
and transl_structure ?cont ~scopes loc fields cc rootpath final_env = function
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
                      | _ -> apply_coercion loc Strict cc (get_field pos))
                    pos_cc_list, loc)
            and id_pos_list =
              List.filter (fun (id,_,_) -> not (Ident.Set.mem id ids))
                id_pos_list
            in
            wrap_id_pos_list loc id_pos_list get_field lam
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
      let str_lam =
        transl_struct_item ~scopes fields rootpath item
          (fun fields ->
             transl_structure ?cont ~scopes loc fields cc rootpath final_env
               rem)
      in
      if in_run_world () || batch_structure_context () then str_lam
      else
        insert_item_splices (TranslSplices.transl_item_splices ~scopes item)
          str_lam

and is_literal_structure me =
  match me.mod_desc with
  | Tmod_structure _ -> true
  | Tmod_constraint (me, _, _, _) -> is_literal_structure me
  | _ -> false

and transl_module_hoisted ~scopes cc rootpath me k =
  match me.mod_desc with
  | Tmod_structure str ->
      transl_struct ~cont:k ~scopes Loc_unknown [] cc rootpath str
  | Tmod_functor (Plain, _, fbody)
    when in_compile_world ()
         && is_mixed_module_type me.mod_env me.mod_type ->
      let saved_mode = !template_apps_mode in
      let saved_idents = Translcore.current_template_batch_idents () in
      with_mixed_body_batch_idents me @@ fun () ->
      with_template_apps Tapps_plain_body @@ fun () ->
      transl_module_hoisted ~scopes (functor_result_coercion cc)
        rootpath fbody
        (fun block ->
           Translcore.with_template_batch_idents saved_idents @@ fun () ->
           with_template_apps saved_mode @@ fun () -> k block)
  | Tmod_constraint (me', _, _, ccarg) ->
      transl_module_hoisted ~scopes (compose_coercions cc ccarg) rootpath
        me' k
  | _ -> k (transl_module ~scopes cc rootpath me)

and transl_template_body ~scopes ~loc params body =
  check_template_body body;
  let depth = List.length params in
  let term_bound = template_bound_idents body in
  let batch_idents =
    let s =
      List.fold_left (fun s x -> Ident.Set.add x s) term_bound params
    in
    match Translcore.current_template_batch_idents () with
    | Some s0 -> Ident.Set.union s0 s
    | None -> s
  in
  Translcore.with_template_batch_idents (Some batch_idents) @@ fun () ->
  let splices = ref [] in
  let collector spl_exp =
    Translcore.check_no_objects spl_exp;
    let h = Ident.create_local "*tsplice*" in
    splices := (h, spl_exp) :: !splices;
    Lvar h
  in
  let apps_acc = ref ([] : template_body_app list) in
  let frag0 =
    in_world Run_world @@ fun () ->
    with_template_apps Tapps_template_body @@ fun () ->
    with_template_body_apps (Some apps_acc) @@ fun () ->
    Translcore.with_splice_source
      (Some (Translcore.Splices_in_place collector))
      (fun () -> transl_module ~scopes Tcoerce_none None body)
  in
  let splices = List.rev !splices in
  let apps = List.rev !apps_acc in
  let term_bound =
    List.fold_left
      (fun s a ->
         let add o s = Option.fold ~none:s ~some:(fun x ->
             Ident.Set.add x s) o in
         add a.tba_lit_funct (add a.tba_lit_arg s))
      term_bound apps
  in
  let body_bound =
    List.fold_left (fun s x -> Ident.Set.add x s) term_bound params in
  let splice_parts =
    List.map (fun (h, spl_exp) ->
        let fvs =
          Ident.Set.union (Translquote.fv spl_exp)
            (expr_macro_call_env_roots spl_exp)
          |> Ident.Set.filter (fun r -> not (Ident.global r))
        in
        (h, spl_exp,
         Ident.Set.inter fvs term_bound,
         Ident.Set.diff fvs body_bound))
      splices
  in
  let app_parts =
    List.map (fun a ->
        let fvs =
          Ident.Set.filter (fun r -> not (Ident.global r))
            (free_variables a.tba_pair_term)
        in
        (a, Ident.Set.inter fvs term_bound,
         Ident.Set.diff fvs body_bound))
      apps
  in
  let roots =
    let free =
      Ident.Set.filter (fun r -> not (Ident.global r))
        (free_variables frag0)
    in
    List.fold_left (fun acc (_, _, _, rs) -> Ident.Set.union acc rs)
      free splice_parts
    |> (fun s ->
          List.fold_left (fun acc (_, _, rs) -> Ident.Set.union acc rs)
            s app_parts)
    |> (fun s ->
          List.fold_left (fun s x -> Ident.Set.remove x s) s params)
    |> Ident.Set.elements
  in
  let eps = Ident.create_local "*tenv*" in
  let env_param = Ident.create_local "*envcode*" in
  let zeros n = List.init n (fun _ -> 0) in
  let me_index =
    List.mapi (fun j x -> (x, 1 :: zeros (depth - j - 1))) params
    @ List.mapi (fun i r -> (r, i :: zeros depth)) roots
  in
  let thunks =
    List.map (fun (h, spl_exp, args, _) ->
        let called_slots =
          Ident.Set.elements
            (Ident.Set.inter (expr_macro_call_env_roots spl_exp)
               term_bound)
        in
        let slot_route =
          List.map (fun r -> (r, Ident.create_local "*tslotcode*"))
            called_slots
        in
        let slot_id_params =
          List.map (fun _ -> Ident.create_local "*tslotid*") called_slots
        in
        let args =
          Ident.Set.diff args (Ident.Set.of_list called_slots)
        in
        let me_param = Ident.create_local "template_env" in
        let raw =
          in_world Run_world @@ fun () ->
          Translcore.with_macro_env
            (Some { Translcore.me_param; me_index })
            (fun () ->
               Translcore.with_template_slot_index (Some slot_route)
                 (fun () -> Translcore.transl_exp ~scopes spl_exp))
        in
        let originals, fresh_vars, code =
          Translquote.transl_close_function Location.none args
            (Translquote.remove_events raw)
        in
        let eps_p = Ident.create_local "*tenvid*" in
        let code =
          List.fold_right2
            (fun (_, code_param) id_param acc ->
               Llet (Strict, Pgenval, code_param,
                     Translquote.quote_expression (Lvar id_param), acc))
            slot_route slot_id_params code
        in
        let code =
          Llet (Strict, Pgenval, me_param,
                Translquote.quote_expression (Lvar eps_p), code)
        in
        let fn =
          lfunction ~kind:Curried
            ~params:
              (List.map (fun v -> (v, Pgenval)) fresh_vars
               @ List.map (fun v -> (v, Pgenval)) slot_id_params
               @ [ (eps_p, Pgenval);
                   (Ident.create_local "*unit*", Pgenval) ])
            ~return:Pgenval ~body:code ~attr:default_function_attribute
            ~loc:Loc_unknown
        in
        let hole =
          Lapply { ap_func = Lvar h;
                   ap_args =
                     List.map (fun r -> Lvar r) originals
                     @ List.map (fun r -> Lvar r) called_slots
                     @ [ Lvar eps; lambda_unit ];
                   ap_loc = Loc_unknown; ap_tailcall = Default_tailcall;
                   ap_inlined = Default_inline;
                   ap_specialised = Default_specialise }
        in
        (h, fn, hole))
      splice_parts
  in
  let app_thunks =
    List.map (fun (a, delivered, _) ->
        let delivered = Ident.Set.elements delivered in
        let id_params =
          List.map (fun _ -> Ident.create_local "*tslotid*") delivered in
        let code_params =
          List.map (fun _ -> Ident.create_local "*tslotcode*") delivered in
        let eps_p = Ident.create_local "*tenvid*" in
        let me_param = Ident.create_local "template_env" in
        let subst_map =
          List.fold_left
            (fun m (r, path) ->
               Ident.Map.add r
                 (List.fold_right
                    (fun i acc ->
                       Lprim (Pfield (i, Pointer, Immutable), [acc],
                              Loc_unknown))
                    path (Lsplice (Lvar me_param)))
                 m)
            Ident.Map.empty me_index
        in
        let subst_map =
          List.fold_left2
            (fun m r cp -> Ident.Map.add r (Lsplice (Lvar cp)) m)
            subst_map delivered code_params
        in
        let inner_env =
          Translquote.quote_expression
            (Lambda.subst (fun _ _ env -> env) subst_map a.tba_pair_term)
        in
        let dyn_call =
          Lapply { ap_func = Lprim (Pfield (1, Pointer, Immutable),
                                    [Lvar a.tba_pair], Loc_unknown);
                   ap_args = [ inner_env ];
                   ap_loc = Loc_unknown; ap_tailcall = Default_tailcall;
                   ap_inlined = Default_inline;
                   ap_specialised = Default_specialise }
        in
        let body =
          Llet (Strict, Pgenval, me_param,
                Translquote.quote_expression (Lvar eps_p),
                List.fold_right2
                  (fun cp idp acc ->
                     Llet (Strict, Pgenval, cp,
                           Translquote.quote_expression (Lvar idp), acc))
                  code_params id_params dyn_call)
        in
        let fn =
          lfunction ~kind:Curried
            ~params:
              (List.map (fun v -> (v, Pgenval)) id_params
               @ [ (eps_p, Pgenval);
                   (Ident.create_local "*unit*", Pgenval) ])
            ~return:Pgenval ~body ~attr:default_function_attribute
            ~loc:Loc_unknown
        in
        let hole =
          Lapply { ap_func = Lvar a.tba_hole;
                   ap_args =
                     List.map (fun r -> Lvar r) delivered
                     @ [ Lvar eps; lambda_unit ];
                   ap_loc = Loc_unknown; ap_tailcall = Default_tailcall;
                   ap_inlined = Default_inline;
                   ap_specialised = Default_specialise }
        in
        (a.tba_hole, fn, hole))
      app_parts
  in
  let thunks = thunks @ app_thunks in
  let frag =
    let rec fill lam =
      match lam with
      | Lsplice (Lvar h) as l ->
          (match
             List.find_map (fun (h', _, hole) ->
                 if Ident.same h h' then Some hole else None)
               thunks
           with
           | Some hole -> Lsplice hole
           | None -> l)
      | Lsplice _ -> lam
      | _ -> Lambda.shallow_map fill lam
    in
    fill frag0
  in
  let frag =
    let project path =
      List.fold_right
        (fun i acc ->
           Lprim (Pfield (i, Pointer, Immutable), [acc], Loc_unknown))
        path (Lvar eps)
    in
    let subst_map =
      List.fold_left (fun m (r, path) -> Ident.Map.add r (project path) m)
        Ident.Map.empty me_index
    in
    Lambda.subst (fun _ _ env -> env) subst_map frag
  in
  let frag =
    Llet (Strict, Pgenval, eps,
          Lsplice (Lprim (Pfield (0, Pointer, Immutable),
                          [Lvar env_param], Loc_unknown)),
          frag)
  in
  let dyn_fn =
    lfunction ~kind:Curried
      ~params:[ (env_param, Pgenval) ]
      ~return:Pgenval
      ~body:
        (List.fold_right
           (fun (h, fn, _) acc -> Llet (Strict, Pgenval, h, fn, acc))
           thunks
           (Translquote.module_builder frag))
      ~attr:default_function_attribute ~loc:Loc_unknown
  in
  let assemble record_block =
    let rec_id = Ident.create_local "*record*" in
    let dyn_id = Ident.create_local "*dyn*" in
    Llet (Strict, Pgenval, rec_id, record_block,
          Llet (Strict, Pgenval, dyn_id, dyn_fn,
                Lprim (Pmakeblock (0, Immutable, None),
                       [Lvar rec_id; Lvar dyn_id], Loc_unknown)))
  in
  let pending_chain = ref ([] : (lambda -> lambda) list) in
  let inner_pair =
    in_world Compile_world @@ fun () ->
    with_template_apps Tapps_template_body @@ fun () ->
    with_template_body_apps (Some apps_acc) @@ fun () ->
    with_template_body_pending_chain (Some pending_chain) @@ fun () ->
    let rec chain_body cc body =
      match body.Typedtree.mod_desc with
      | Tmod_structure str ->
          transl_struct ~cont:assemble ~scopes Loc_unknown [] cc None str
      | Tmod_constraint (b, _, _, ccarg) ->
          chain_body (compose_coercions cc ccarg) b
      | _ ->
          assemble (transl_module ~scopes cc None body)
    in
    chain_body Tcoerce_none body
  in
  let inner_pair =
    List.fold_left (fun acc wrap -> wrap acc) inner_pair !pending_chain
  in
  let wrap_level ~xm inner =
    let rec_id = Ident.create_local "*record*" in
    let dyn_id = Ident.create_local "*dyn*" in
    let outer_env = Ident.create_local "*envcode*" in
    let context_dyn =
      lfunction ~kind:Curried ~params:[ (outer_env, Pgenval) ]
        ~return:Pgenval
        ~body:
          (Translquote.module_builder
             (Lsplice (Lprim (Pfield (0, Pointer, Immutable),
                              [Lvar outer_env], Loc_unknown))))
        ~attr:default_function_attribute ~loc:Loc_unknown
    in
    lfunction ~kind:Curried ~params:[ (xm, Pgenval) ]
      ~return:Pgenval
      ~body:
        (Llet (Strict, Pgenval, rec_id, inner,
               Llet (Strict, Pgenval, dyn_id, context_dyn,
                     Lprim (Pmakeblock (0, Immutable, None),
                            [Lvar rec_id; Lvar dyn_id], Loc_unknown))))
      ~attr:default_function_attribute ~loc:(of_location ~scopes loc)
  in
  let xms = List.map (fun _ -> Ident.create_local "*xmacros*") params in
  let outer_xms = List.filteri (fun j _ -> j < depth - 1) xms in
  let innermost_fn =
    let xm = List.nth xms (depth - 1) in
    lfunction ~kind:Curried ~params:[ (xm, Pgenval) ]
      ~return:Pgenval ~body:inner_pair ~attr:default_function_attribute
      ~loc:(of_location ~scopes loc)
  in
  let f_fn =
    List.fold_right (fun xm acc -> wrap_level ~xm acc)
      outer_xms innermost_fn
  in
  let f_fn =
    Lambda.subst (fun _ _ env -> env)
      (List.fold_left2 (fun m x xm -> Ident.Map.add x (Lvar xm) m)
         Ident.Map.empty params xms)
      f_fn
  in
  { tt_roots = roots; tt_function = f_fn }

and template_argument_root ~scopes ?lit m_id (arg : Typedtree.module_expr) =
  match lit with
  | Some id -> Lvar id
  | None ->
      if Translcore.in_toplevel () && in_compile_world () then
        toplevel_template_argument_view ~scopes arg
      else if is_module_path arg then
        let block = transl_module ~scopes Tcoerce_none None arg in
        if Translcore.in_toplevel () then
          toplevel_run_time_argument arg block
        else block
      else Lvar (template_application_idents m_id).ta_arg

and toplevel_template_argument_view ~scopes (arg : Typedtree.module_expr) =
  match module_expr_path arg with
  | None ->
      raise (Error (arg.mod_loc, Template_restriction
        "In the toplevel, a template functor argument must be a path."))
  | Some path ->
      let env = arg.mod_env in
      let path =
        try Env.normalize_module_path None env path
        with Not_found -> path
      in
      (match path with
       | Pident id when Translcore.is_toplevel_template_module id ->
           toploop_getvalue_key (toplevel_record_key id)
       | _ when Translcore.in_template_batch_set (Path.head path) ->
           transl_module ~scopes Tcoerce_none None arg
       | _ when Ident.global (Path.head path) ->
           transl_module_macros_path Loc_unknown env path
       | _ ->
           toplevel_compile_time_view arg
             (transl_module ~scopes Tcoerce_none None arg))

and template_functor_root ~scopes ?lit m_id (funct : Typedtree.module_expr) =
  match lit with
  | Some id -> Lvar id
  | None ->
      if Translcore.in_toplevel () && in_compile_world () then
        toplevel_template_functor_component ~scopes funct
      else if is_module_path funct then
        transl_module ~scopes Tcoerce_none None funct
      else Lvar (template_application_idents m_id).ta_funct

and toplevel_template_functor_component ~scopes
      (funct : Typedtree.module_expr) =
  match module_expr_path funct with
  | None ->
      raise (Error (funct.mod_loc, Template_restriction
        "In the toplevel, the applied template functor must be a path."))
  | Some path ->
      let env = funct.mod_env in
      let path =
        try Env.normalize_module_path None env path
        with Not_found -> path
      in
      (match path with
       | _ when Translcore.in_template_batch_set (Path.head path) ->
           transl_module ~scopes Tcoerce_none None funct
       | Pident id when Ident.Set.mem id !toplevel_template_functors ->
           toploop_getvalue_key (template_component_key id)
       | Pident id when Translcore.is_toplevel_template_module id ->
           toploop_getvalue_key (toplevel_record_key id)
       | Pdot _
         when Translcore.is_toplevel_template_module (Path.head path) ->
           let rec reroot base = function
             | Env.Aident _ -> base
             | Env.Adot (a, pos) ->
                 Lprim (Pfield (pos, Pointer, Immutable),
                        [reroot base a], Loc_unknown)
           in
           reroot
             (toploop_getvalue_key
                (toplevel_record_key (Path.head path)))
             (Env.find_module_address path env)
       | _ when Ident.global (Path.head path) ->
           transl_module_macros_path Loc_unknown env path
       | _ ->
           raise (Error (funct.mod_loc, Template_restriction
             "In the toplevel, the applied template functor must be \
              defined by a toplevel phrase or come from a compiled \
              unit.")))

and bind_literal_template_functor ~scopes ?(transform = Fun.id) ~loc m_id
      funct inner =
  if is_module_path funct then inner
  else begin
    let params, fbody = peel_template_params funct in
    let subscopes = enter_module_definition ~scopes m_id in
    let tt =
      transl_template_body ~scopes:subscopes ~loc params fbody
    in
    let bound =
      if in_run_world () then
        macro_env_of_roots tt.tt_roots
      else tt.tt_function
    in
    Llet (Strict, Pgenval, (template_application_idents m_id).ta_funct,
          transform bound, inner)
  end

and bind_literal_template_argument ~scopes ?(transform = Fun.id) m_id argopt
      inner =
  match argopt with
  | Some (arg, _) when not (is_module_path arg) ->
      let ta_arg = (template_application_idents m_id).ta_arg in
      if in_compile_world () then begin
        if Mtype.has_macro_components arg.mod_env arg.mod_type
           || literal_argument_compile_parts arg
        then
          with_mixed_body_batch_idents arg @@ fun () ->
          transl_module_hoisted ~scopes Tcoerce_none None arg
            (fun block ->
               Llet (Strict, Pgenval, ta_arg, transform block, inner))
        else inner
      end
      else
        Llet (Strict, Pgenval, ta_arg,
              transform
                (with_mixed_body_batch_idents arg @@ fun () ->
                 transl_module ~scopes Tcoerce_none None arg),
              inner)
  | _ -> inner

and template_component_call ~scopes ~loc ?lit_funct ?lit_arg m_id funct
      argopt =
  let f_component = template_functor_root ~scopes ?lit:lit_funct m_id funct in
  let x_macros =
    match argopt with
    | Some (arg, ccarg)
      when Mtype.has_macro_components arg.mod_env arg.mod_type ->
        apply_coercion (of_location ~scopes loc) Strict ccarg
          (template_argument_root ~scopes ?lit:lit_arg m_id arg)
    | _ -> lambda_unit
  in
  Lapply { ap_func = f_component; ap_args = [ x_macros ];
           ap_loc = of_location ~scopes loc;
           ap_tailcall = Default_tailcall; ap_inlined = Default_inline;
           ap_specialised = Default_specialise }

and template_env_pair_term ~scopes ~loc ?lit_funct ?lit_arg m_id funct
      argopt =
  let f_slot = template_functor_root ~scopes ?lit:lit_funct m_id funct in
  let arg_block =
    match argopt with
    | Some (arg, ccarg) ->
        apply_coercion (of_location ~scopes loc) Strict ccarg
          (template_argument_root ~scopes ?lit:lit_arg m_id arg)
    | None -> lambda_unit
  in
  Lprim (Pmakeblock (0, Immutable, None), [f_slot; arg_block],
         Loc_unknown)

and template_env_code ~scopes ~loc m_id funct argopt =
  let pair_term =
    in_world Run_world @@ fun () ->
    template_env_pair_term ~scopes ~loc m_id funct argopt
  in
  let pair_term =
    match Translcore.current_native_slot_layout () with
    | Some nsl -> Translcore.name_slots_through_block nsl pair_term
    | None -> pair_term
  in
  let roots =
    Ident.Set.filter (fun r -> not (Ident.global r))
      (free_variables pair_term)
  in
  Translquote.transl_close_quotation Location.none roots
    (Translquote.quote_expression pair_term)

and transl_body_application ~scopes acc mb fields next =
  let m_id, funct, argopt = template_application_shape mb in
  let next fields' =
    if mb.Typedtree.mb_id = None then next fields else next fields' in
  let literal_functor_translation () =
    let params, fbody = peel_template_params funct in
    let subscopes = enter_module_definition ~scopes m_id in
    transl_template_body ~scopes:subscopes ~loc:mb.Typedtree.mb_loc
      params fbody
  in
  if in_run_world () then begin
    let lit_funct =
      if is_module_path funct then None
      else Some (Ident.create_local "*tbfunct*")
    in
    let lit_arg =
      match argopt with
      | Some (arg, _) when not (is_module_path arg) ->
          Some (Ident.create_local "*tbarg*")
      | _ -> None
    in
    let hole = Ident.create_local "*tapphole*" in
    let pair = Ident.create_local "*tapppair*" in
    let pair_term =
      template_env_pair_term ~scopes ~loc:mb.Typedtree.mb_loc
        ?lit_funct ?lit_arg m_id funct argopt
    in
    acc := { tba_m_id = m_id; tba_hole = hole; tba_pair = pair;
             tba_pair_term = pair_term;
             tba_lit_funct = lit_funct; tba_lit_arg = lit_arg } :: !acc;
    let item =
      Llet (Strict, Pgenval, m_id,
            apply_coercion Loc_unknown Strict
              (template_application_rescc mb.Typedtree.mb_expr)
              (Lsplice (Lvar hole)),
            next (m_id :: fields))
    in
    let item =
      match lit_arg, argopt with
      | Some la, Some (arg, _) ->
          Llet (Strict, Pgenval, la,
                transl_module ~scopes Tcoerce_none None arg, item)
      | _ -> item
    in
    match lit_funct with
    | None -> item
    | Some lf ->
        let tt = literal_functor_translation () in
        let tuple =
          macro_env_of_roots tt.tt_roots
        in
        Llet (Strict, Pgenval, lf, tuple, item)
  end
  else begin
    let entry =
      match
        List.find_opt (fun a -> Ident.same a.tba_m_id m_id) !acc
      with
      | Some e -> e
      | None ->
          Misc.fatal_error
            "Translmod.transl_body_application: uncollected application"
    in
    if !template_apps_mode = Tapps_template_plain_body then begin
      let pending =
        match !template_body_pending_chain with
        | Some r -> r
        | None ->
            Misc.fatal_error
              "Translmod.transl_body_application: no pending chain"
      in
      (match entry.tba_lit_funct with
       | None -> ()
       | Some lf ->
           let tt = literal_functor_translation () in
           pending :=
             (fun inner -> Llet (Strict, Pgenval, lf, tt.tt_function,
                                 inner))
             :: !pending);
      (match entry.tba_lit_arg, argopt with
       | Some la, Some (arg, _)
         when Mtype.has_macro_components arg.mod_env arg.mod_type
              || literal_argument_compile_parts arg ->
           let placeholder = Ident.create_local "*tlithole*" in
           let wrapped =
             transl_module_hoisted ~scopes Tcoerce_none None arg
               (fun block ->
                  Llet (Strict, Pgenval, la, block, Lvar placeholder))
           in
           pending :=
             (fun inner ->
                Lambda.subst (fun _ _ e -> e)
                  (Ident.Map.singleton placeholder inner) wrapped)
             :: !pending
       | _ -> ());
      let call =
        template_component_call ~scopes ~loc:mb.Typedtree.mb_loc
          ?lit_funct:entry.tba_lit_funct ?lit_arg:entry.tba_lit_arg
          m_id funct argopt
      in
      pending :=
        (fun inner ->
           Llet (Strict, Pgenval, entry.tba_pair, call, inner))
        :: !pending;
      Llet (Strict, Pgenval, m_id,
            apply_coercion Loc_unknown Strict
              (template_application_rescc mb.Typedtree.mb_expr)
              (Lprim (Pfield (0, Pointer, Immutable),
                      [Lvar entry.tba_pair], Loc_unknown)),
            next (m_id :: fields))
    end else
    let item =
      Llet (Strict, Pgenval, entry.tba_pair,
            template_component_call ~scopes ~loc:mb.Typedtree.mb_loc
              ?lit_funct:entry.tba_lit_funct ?lit_arg:entry.tba_lit_arg
              m_id funct argopt,
            Llet (Strict, Pgenval, m_id,
                  apply_coercion Loc_unknown Strict
                    (template_application_rescc mb.Typedtree.mb_expr)
                    (Lprim (Pfield (0, Pointer, Immutable),
                            [Lvar entry.tba_pair], Loc_unknown)),
                  next (m_id :: fields)))
    in
    let item =
      match entry.tba_lit_arg, argopt with
      | Some la, Some (arg, _)
        when Mtype.has_macro_components arg.mod_env arg.mod_type
             || literal_argument_compile_parts arg ->
          transl_module_hoisted ~scopes Tcoerce_none None arg
            (fun block -> Llet (Strict, Pgenval, la, block, item))
      | _ -> item
    in
    match entry.tba_lit_funct with
    | None -> item
    | Some lf ->
        let tt = literal_functor_translation () in
        Llet (Strict, Pgenval, lf, tt.tt_function, item)
  end

and transl_application_binding ~scopes ~loc ~rescc ~mixed_result m_id
      funct argopt ~dummies fields next inner =
  let mcode = template_application_slot m_id in
  let coerced lam = apply_coercion (of_location ~scopes loc) Strict rescc lam in
  let bind_literals body =
    bind_literal_template_functor ~scopes ~loc m_id funct
      (bind_literal_template_argument ~scopes m_id argopt body)
  in
  if in_run_world () then
    bind_literals
      (Llet (Strict, Pgenval, m_id, coerced (Lsplice (Lvar mcode)),
             inner ()))
  else if !translating_macros_object
          || (mixed_result && !template_apps_mode = Tapps_plain_body)
  then
    bind_literals
      (Llet (Strict, Pgenval, m_id,
             coerced
               (Lprim (Pfield (0, Pointer, Immutable),
                       [ template_component_call ~scopes ~loc m_id funct
                           argopt ],
                       Loc_unknown)),
             inner ()))
  else if !template_apps_mode = Tapps_plain_body then
    dummy_slots dummies next fields
  else
    let pr = Ident.create_local "*tpair*" in
    let field i =
      Lprim (Pfield (i, Pointer, Immutable), [Lvar pr], Loc_unknown)
    in
    bind_literals
      (Llet (Strict, Pgenval, pr,
            template_component_call ~scopes ~loc m_id funct argopt,
        Llet (Strict, Pgenval, m_id, coerced (field 0),
          Llet (Strict, Pgenval, mcode,
                Lapply { ap_func = field 1;
                         ap_args =
                           [ template_env_code ~scopes ~loc m_id funct
                               argopt ];
                         ap_loc = Loc_unknown;
                         ap_tailcall = Default_tailcall;
                         ap_inlined = Default_inline;
                         ap_specialised = Default_specialise },
                inner ()))))

and transl_struct_item ~scopes fields rootpath item next =
  match item.str_desc with
  | Tstr_eval (expr, _) ->
      let body = next fields in
      if in_run_world () then Lsequence(transl_exp ~scopes expr, body) else body
  | Tstr_value(rec_flag, st_lev, pat_expr_list)
    when in_run_world () && st_lev < 0 && batch_structure_context () ->
      let ids = List.map macro_bound_id pat_expr_list in
      let body = next (List.rev_append ids fields) in
      macro_env_slots rec_flag pat_expr_list body
  | Tstr_value(rec_flag, st_lev, pat_expr_list)
    when in_compile_world () && st_lev < 0 && batch_structure_context () ->
      let translated =
        List.map (fun (id, _, fn) -> (id, fn))
          (transl_macro_group_functions ~scopes rec_flag pat_expr_list)
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
  | Tstr_value(rec_flag, st_lev, pat_expr_list)
    when st_lev < 0 && not (batch_structure_context ()) ->
      let ext_fields =
        List.rev_append (let_bound_idents pat_expr_list) fields in
      toplevel_macro_pairs ~scopes rec_flag pat_expr_list @@ fun pairs ->
      List.fold_right
        (fun (id, pair) body -> Llet (Strict, Pgenval, id, pair, body))
        pairs (next ext_fields)
  | Tstr_value(rec_flag, st_lev, pat_expr_list)
    when in_run_world () || st_lev < 0 ->
      (* Translate bindings first *)
      let mk_lam_let =
        transl_let ~scopes ~in_structure:true rec_flag pat_expr_list in
      let ext_fields =
        List.rev_append (let_bound_idents pat_expr_list) fields in
      (* Then, translate remainder of struct *)
      let body = next ext_fields in
      mk_lam_let body
  | Tstr_value (_, _, pat_expr_list) ->
      dummy_slots (let_bound_idents pat_expr_list) next fields
  | Tstr_primitive descr ->
      (if in_run_world () then record_primitive descr.val_val);
      next fields
  | Tstr_type _ ->
      next fields
  | Tstr_typext(tyext) ->
      let ids = List.map (fun ext -> ext.ext_id) tyext.tyext_constructors in
      let body = next (List.rev_append ids fields) in
      transl_type_extension ~scopes item.str_env rootpath tyext body
  | Tstr_exception ext ->
      let id = ext.tyexn_constructor.ext_id in
      let path = field_path rootpath id in
      let body = next (id::fields) in
      Llet(Strict, Pgenval, id,
           transl_extension_constructor ~scopes
             item.str_env
             path
             ext.tyexn_constructor, body)
  | Tstr_module ({mb_presence=Mp_present} as mb)
    when batch_structure_context ()
         && template_functor_parts mb.mb_expr <> None ->
      let params, fbody = peel_template_params mb.mb_expr in
      let id = mb.mb_id in
      let subscopes = match id with
        | None -> scopes
        | Some id -> enter_module_definition ~scopes id in
      let tt =
        transl_template_body ~scopes:subscopes ~loc:mb.mb_loc params fbody
      in
      let bound =
        if in_run_world () then
          macro_env_of_roots tt.tt_roots
        else tt.tt_function
      in
      let body = next (cons_opt id fields) in
      begin match id with
      | None ->
          Lsequence (Lprim (Pignore, [bound],
                            of_location ~scopes mb.mb_name.loc), body)
      | Some id -> Llet (Strict, Pgenval, id, bound, body)
      end
  | Tstr_module ({mb_presence=Mp_present} as mb)
    when batch_structure_context ()
         && template_application_parts mb.mb_expr <> None
         && Option.is_some !template_body_apps ->
      transl_body_application ~scopes (Option.get !template_body_apps)
        mb fields next
  | Tstr_module ({mb_presence=Mp_present} as mb)
    when not (Translcore.in_toplevel ())
         && template_application_parts mb.mb_expr <> None ->
      let m_id, funct, argopt = template_application_checks mb in
      let dummies, k =
        if mb.mb_id = None then [], (fun () -> next fields)
        else [m_id], (fun () -> next (m_id :: fields))
      in
      transl_application_binding ~scopes ~loc:mb.mb_loc
        ~rescc:(template_application_rescc mb.mb_expr)
        ~mixed_result:(Mtype.has_macro_components mb.mb_expr.mod_env
                         mb.mb_expr.mod_type)
        m_id funct argopt ~dummies fields next
        k
  | Tstr_include incl
    when batch_structure_context ()
         && Option.is_some !template_body_apps
         && template_application_parts incl.incl_mod <> None ->
      let acc = Option.get !template_body_apps in
      let m_id = anonymous_application_id incl.incl_loc in
      let ids = bound_value_identifiers incl.incl_type in
      let macro_ids =
        List.filter_map
          (function
            | Types.Sig_value (id, { val_staging_level = l; _ }, _)
              when l < 0 -> Some id
            | Types.Sig_module (id, _, md, _, _)
              when Mtype.has_macro_components incl.incl_mod.mod_env
                     md.md_type -> Some id
            | _ -> None)
          incl.incl_type
        |> Ident.Set.of_list
      in
      let mb : Typedtree.module_binding =
        { mb_id = None;
          mb_name = { txt = None; loc = incl.incl_loc };
          mb_uid = Shape.Uid.internal_not_actually_unique;
          mb_presence = Mp_present;
          mb_expr = incl.incl_mod;
          mb_attributes = [];
          mb_loc = incl.incl_loc }
      in
      transl_body_application ~scopes acc mb fields
        (fun fields ->
           if in_compile_world () then
             let rec rebind pos fields = function
               | [] -> next fields
               | id :: rest ->
                   let body = rebind (pos + 1) (id :: fields) rest in
                   if Ident.Set.mem id macro_ids then
                     Llet (Strict, Pgenval, id,
                           Lprim (Pfield (pos, Pointer, Mutable),
                                  [Lvar m_id], Loc_unknown),
                           body)
                   else
                     Llet (Alias, Pgenval, id, Lconst const_unit, body)
             in
             rebind 0 fields ids
           else
             let rec rebind pos fields = function
               | [] -> next fields
               | id :: rest ->
                   Llet (Alias, Pgenval, id,
                         Lprim (Pfield (pos, Pointer, Mutable),
                                [Lvar m_id], Loc_unknown),
                         rebind (pos + 1) (id :: fields) rest)
             in
             rebind 0 fields ids)
  | Tstr_include incl
    when not (Translcore.in_toplevel ())
         && template_application_parts incl.incl_mod <> None ->
      let funct, argopt =
        Option.get (template_application_parts incl.incl_mod)
      in
      let loc = incl.incl_loc in
      template_application_mode_check ~loc;
      template_application_shape_checks ~loc funct;
      let m_id = template_include_binder loc in
      let ids = bound_value_identifiers incl.incl_type in
      let rebind_idents fields ids =
        rebind_fields ~scopes ~loc ~block:m_id ids fields next
      in
      transl_application_binding ~scopes ~loc
        ~rescc:(template_application_rescc incl.incl_mod)
        ~mixed_result:(Mtype.has_macro_components incl.incl_mod.mod_env
                         incl.incl_mod.mod_type)
        m_id funct argopt ~dummies:ids fields next
        (fun () -> rebind_idents fields ids)
  | Tstr_module ({mb_presence=Mp_present} as mb)
    when in_compile_world () && not (Translcore.in_toplevel ())
         && not !translating_macros_object
         && !template_apps_mode = Tapps_items
         && is_plain_functor mb.mb_expr
         && module_has_template_application mb.mb_expr ->
      let apps = plain_body_template_applications mb.mb_expr in
      let locals =
        bound_module_idents_of_module !template_plain_body_locals mb.mb_expr
      in
      let hoisted body =
        List.fold_right
          (fun { pba_loc = loc; pba_m_id = m_id;
                 pba_funct = funct; pba_argopt = argopt } body ->
             template_plain_body_path_checks ~locals ~loc funct;
             let mcode = template_application_slot m_id in
             let pr = Ident.create_local "*tpair*" in
             bind_literal_template_functor ~scopes ~loc m_id funct
               (bind_literal_template_argument ~scopes m_id argopt
                  (Llet (Strict, Pgenval, pr,
                         template_component_call ~scopes ~loc m_id funct
                           argopt,
                    Llet (Strict, Pgenval, mcode,
                          Lapply { ap_func =
                                     Lprim (Pfield (1, Pointer, Immutable),
                                            [Lvar pr], Loc_unknown);
                                   ap_args =
                                     [ template_env_code ~scopes ~loc
                                         m_id funct argopt ];
                                   ap_loc = Loc_unknown;
                                   ap_tailcall = Default_tailcall;
                                   ap_inlined = Default_inline;
                                   ap_specialised = Default_specialise },
                          body)))))
          apps body
      in
      let id = mb.mb_id in
      let subscopes = match id with
        | None -> scopes
        | Some id -> enter_module_definition ~scopes id in
      transl_module_hoisted ~scopes:subscopes Tcoerce_none
        (Option.bind id (field_path rootpath)) mb.mb_expr
        (fun module_body ->
           let body = next (cons_opt id fields) in
           hoisted
             (match id with
              | None ->
                  Lsequence (Lprim (Pignore, [module_body],
                                    of_location ~scopes mb.mb_name.loc), body)
              | Some id ->
                  Llet (pure_module mb.mb_expr, Pgenval, id, module_body,
                        body)))
  | Tstr_module ({mb_presence=Mp_present} as mb)
    when in_compile_world ()
         && (batch_structure_context ()
             || (Translcore.in_toplevel ()
                 && is_literal_structure mb.mb_expr))
         && (is_literal_structure mb.mb_expr
             || (is_plain_functor mb.mb_expr
                 && is_mixed_module_type mb.mb_expr.mod_env
                      mb.mb_expr.mod_type)) ->
      let id = mb.mb_id in
      let subscopes = match id with
        | None -> scopes
        | Some id -> enter_module_definition ~scopes id in
      transl_module_hoisted ~scopes:subscopes Tcoerce_none
        (Option.bind id (field_path rootpath)) mb.mb_expr
        (fun block ->
           let body = next (cons_opt id fields) in
           match id with
           | None ->
               Lsequence (Lprim (Pignore, [block],
                                 of_location ~scopes mb.mb_name.loc), body)
           | Some id -> Llet (Strict, Pgenval, id, block, body))
  | Tstr_module ({mb_presence=Mp_present} as mb)
    when in_compile_world () && batch_structure_context ()
         && not (Mtype.has_macro_components mb.mb_expr.mod_env
                   mb.mb_expr.mod_type)
         && not (module_has_template_application mb.mb_expr) ->
      dummy_slots (Option.to_list mb.mb_id) next fields
  | Tstr_module ({mb_presence=Mp_present} as mb)
    when in_compile_world ()
         && !template_apps_mode = Tapps_template_body
         && is_plain_functor mb.mb_expr
         && not (is_mixed_module_type mb.mb_expr.mod_env
                   mb.mb_expr.mod_type)
         && module_has_template_application mb.mb_expr ->
      let pending = ref ([] : (lambda -> lambda) list) in
      let id = mb.mb_id in
      let subscopes = match id with
        | None -> scopes
        | Some id -> enter_module_definition ~scopes id in
      let module_body =
        with_template_body_pending_chain (Some pending) @@ fun () ->
        transl_module ~scopes:subscopes Tcoerce_none
          (Option.bind id (field_path rootpath)) mb.mb_expr
      in
      let body = next (cons_opt id fields) in
      let item =
        match id with
        | None ->
            Lsequence (Lprim (Pignore, [module_body],
                              of_location ~scopes mb.mb_name.loc), body)
        | Some id ->
            Llet (pure_module mb.mb_expr, Pgenval, id, module_body, body)
      in
      List.fold_left (fun acc wrap -> wrap acc) item !pending
  | Tstr_module ({mb_presence=Mp_present} as mb) ->
      let id = mb.mb_id in
      (* Translate module first *)
      let subscopes = match id with
        | None -> scopes
        | Some id -> enter_module_definition ~scopes id in
      let module_body =
        transl_module ~scopes:subscopes Tcoerce_none
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
  | Tstr_recmodule bindings
    when in_compile_world () && batch_structure_context ()
         && List.for_all
              (fun mb ->
                 not (Mtype.has_macro_components mb.mb_expr.mod_env
                        mb.mb_expr.mod_type)
                 && not (module_has_template_application mb.mb_expr))
              bindings ->
      dummy_slots (List.filter_map (fun mb -> mb.mb_id) bindings) next fields
  | Tstr_recmodule bindings ->
      let ext_fields =
        List.rev_append (List.filter_map (fun mb -> mb.mb_id) bindings)
          fields
      in
      let body = next ext_fields in
      let lam =
        compile_recmodule ~scopes (fun id modl ->
            match id with
            | None -> transl_module ~scopes Tcoerce_none None modl
            | Some id ->
                transl_module
                  ~scopes:(enter_module_definition ~scopes id)
                  Tcoerce_none (field_path rootpath id) modl
          ) bindings body
      in
      lam
  | Tstr_class cl_list when in_compile_world () ->
      dummy_slots (List.map (fun (ci, _) -> ci.ci_id_class) cl_list)
        next fields
  | Tstr_class cl_list ->
      let (ids, class_bindings) = transl_class_bindings ~scopes cl_list in
      let body = next (List.rev_append ids fields) in
      Value_rec_compiler.compile_letrec class_bindings body
  | Tstr_include incl ->
      if in_compile_world () && not (is_literal_structure incl.incl_mod)
      && not (Translcore.in_toplevel ())
      && not (Mtype.has_macro_components incl.incl_mod.mod_env
                (Mty_signature incl.incl_type))
      then
        dummy_slots (bound_value_identifiers incl.incl_type) next fields
      else
      let ids = bound_value_identifiers incl.incl_type in
      let modl = incl.incl_mod in
      let mid = Ident.create_local "include" in
      let rebind_idents fields ids =
        rebind_fields ~scopes ~loc:incl.incl_loc ~block:mid ids fields next
      in
      if in_compile_world ()
         && (batch_structure_context () || Translcore.in_toplevel ())
         && is_literal_structure modl
      then
        transl_module_hoisted ~scopes Tcoerce_none None modl
          (fun block ->
             Llet (Strict, Pgenval, mid, block, rebind_idents fields ids))
      else
      Llet(pure_module modl, Pgenval, mid,
           transl_module ~scopes Tcoerce_none None modl,
           rebind_idents fields ids)
  | Tstr_open od ->
      if in_compile_world ()
         && (Translcore.in_toplevel ()
             || (not (Mtype.has_macro_components od.open_expr.mod_env
                        (Mty_signature od.open_bound_items))
                 && not (module_has_template_application od.open_expr)))
      then
        dummy_slots (bound_value_identifiers od.open_bound_items) next fields
      else
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
          if in_compile_world () && is_literal_structure od.open_expr then
            transl_module_hoisted ~scopes Tcoerce_none None od.open_expr
              (fun block ->
                 Llet (Strict, Pgenval, mid, block,
                       rebind_idents fields ids))
          else
          let body = rebind_idents fields ids in
          Llet(pure, Pgenval, mid,
               transl_module ~scopes Tcoerce_none None od.open_expr, body)
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

let without_typing_required_globals f =
  let saved = Env.get_required_globals () in
  Env.reset_required_globals ();
  let result = f () in
  List.iter Env.add_required_global saved;
  result

(* Compile an implementation *)

let module_block_size component_names coercion =
  match coercion with
  | Tcoerce_none -> List.length component_names
  | Tcoerce_structure (l, _) -> List.length l
  | Tcoerce_functor _
  | Tcoerce_primitive _
  | Tcoerce_alias _ -> assert false

let transl_implementation_flambda module_name (str, cc) =
  reset_labels ();
  primitive_declarations := [];
  Translprim.clear_used_primitives ();
  let module_id = Ident.create_persistent module_name in
  let scopes = enter_module_definition ~scopes:empty_scopes module_id in
  let body =
    Translobj.transl_label_init
      (fun () -> transl_struct ~scopes Loc_unknown [] cc
                   (global_path module_id) str)
  in
  let size =
    module_block_size (bound_value_identifiers str.str_type) cc in
  { module_ident = module_id;
    main_module_block_size = size;
    required_globals = required_globals ~flambda:true body;
    code = body }

let transl_implementation module_name (str, cc) =
  let implementation =
    transl_implementation_flambda module_name (str, cc)
  in
  let code =
    Lprim (Psetglobal implementation.module_ident, [implementation.code],
           Loc_unknown)
  in
  { implementation with code }

let macros_global module_name =
  Ident.create_persistent (Unit_info.macros_modname module_name)

let transl_store_implementation_fwd =
  ref ((fun _ _ -> assert false)
       : string -> structure * module_coercion -> Lambda.program)

let store_ident_positions_fwd =
  ref ((fun _ -> assert false)
       : structure * module_coercion -> Ident.t -> int option)

let call_static_support m args =
  let unit = Lprim (Pgetglobal (Ident.create_persistent m), [], Loc_unknown) in
  Lapply { ap_func = Lprim (Pfield (0, Pointer, Immutable), [unit],
                            Loc_unknown);
           ap_args = args;
           ap_loc = Loc_unknown;
           ap_tailcall = Default_tailcall;
           ap_inlined = Default_inline;
           ap_specialised = Default_specialise }

let rec exports_macros str =
  List.exists
    (fun item -> match item.str_desc with
       | Tstr_value (_, st_lev, _) -> st_lev < 0
       | Tstr_module mb -> module_exports_macros mb.mb_expr
       | Tstr_recmodule mbs ->
           List.exists (fun mb -> module_exports_macros mb.mb_expr) mbs
       | Tstr_include incl ->
           module_exports_macros incl.incl_mod
           || Mtype.has_macro_components incl.incl_mod.mod_env
                (Mty_signature incl.incl_type)
       | _ -> false)
    str.str_items

and module_exports_macros mexp =
  match mexp.mod_desc with
  | Tmod_structure str -> exports_macros str
  | Tmod_constraint (mexp, _, _, _) -> module_exports_macros mexp
  | Tmod_functor (Plain, _, _)
  | Tmod_apply (Plain, _, _, _) | Tmod_apply_unit (Plain, _) ->
      Mtype.has_macro_components mexp.mod_env mexp.mod_type
  | Tmod_functor (Template, _, _) ->
      true
  | Tmod_apply (Template, _, _, _) | Tmod_apply_unit (Template, _) ->
      Mtype.has_macro_components mexp.mod_env mexp.mod_type
  | _ -> false

let transl_macros_object module_name (str, cc) =
  protect_refs [R (Clflags.native_code, false)] @@ fun () ->
  let module_id = Ident.create_persistent module_name in
  let macros_id = macros_global module_name in
  let scopes = enter_module_definition ~scopes:empty_scopes module_id in
  let saved_tmo = !translating_macros_object in
  translating_macros_object := true;
  Fun.protect
    ~finally:(fun () -> translating_macros_object := saved_tmo)
  @@ fun () ->
  let body, required =
    without_typing_required_globals @@ fun () ->
      let body =
        at_compile_world @@ fun () ->
          let mod_body =
            transl_struct ~scopes Loc_unknown [] cc
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

let quoted_builders_requested =
  lazy (Sys.getenv_opt "MACOCAML_QUOTED_BUILDERS" <> None)

let quoted_builders () = Lazy.force quoted_builders_requested

let transl_static_program ?(native = false) module_name (str, cc)
    ~source_file ~prefix =
  let module_id = Ident.create_persistent module_name in
  let scopes = enter_module_definition ~scopes:empty_scopes module_id in

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
             nsl_pos = !store_ident_positions_fwd (str, cc) }
    else None
  in
  Translcore.with_native_slot_layout slot_layout @@ fun () ->
  without_typing_required_globals @@ fun () ->
  (reset_template_applications ();
   template_apps_mode := Tapps_items);
  Fun.protect ~finally:(fun () -> template_apps_mode := Tapps_inactive)
  @@ fun () ->

  let pending_splices = ref [] in
  let splice_thunk spl_exp =
    protect_refs [R (Clflags.native_code, false)] @@ fun () ->
    Translcore.check_no_objects spl_exp;
    let roots =
      let roots = Translquote.fv spl_exp in
      match slot_layout with
      | Some nsl ->
          Ident.Set.filter (fun r -> nsl.Translcore.nsl_pos r = None) roots
      | None -> roots
    in
    let originals, fresh_vars, code =
      Translquote.transl_close_function Location.none roots
        (Translcore.transl_exp ~scopes spl_exp)
    in
    let code = static_report spl_exp.exp_loc "this splice" code in
    let h = Ident.create_local "*splice*" in
    let fn =
      lfunction ~kind:Curried
        ~params:
          (List.map (fun v -> (v, Pgenval)) fresh_vars
           @ [ (Ident.create_local "*unit*", Pgenval) ])
        ~return:Pgenval ~body:code ~attr:default_function_attribute
        ~loc:Loc_unknown
    in
    pending_splices := (h, fn) :: !pending_splices;
    Lapply { ap_func = Lvar h;
             ap_args = List.map (fun r -> Lvar r) originals @ [ lambda_unit ];
             ap_loc = Loc_unknown; ap_tailcall = Default_tailcall;
             ap_inlined = Default_inline;
             ap_specialised = Default_specialise }
  in
  let run_time =
    set_splice_source (Some (Splices_in_place splice_thunk));
    Fun.protect ~finally:(fun () -> set_splice_source None)
      (fun () ->
         if not native then transl_implementation module_name (str, cc)
         else if Config.flambda then
           transl_implementation_flambda module_name (str, cc)
         else !transl_store_implementation_fwd module_name (str, cc))
  in

  let rt_required_globals =
    let rec blank lam =
      match lam with
      | Lsplice _ -> Lsplice lambda_unit
      | _ -> Lambda.shallow_map blank lam
    in
    required_globals ~flambda:false (blank run_time.code)
  in
  let imports = Env.imports () in
  let primitives = !primitive_declarations in
  let flags = Clflags.emitter_flags () in
  let emit =
    let term =
      call_static_support "Static_simplif"
        [if quoted_builders ()
         then Translquote.module_builder run_time.code
         else Translquote.quote_module_lambda run_time.code]
    in
    let emit_call =
    if native then
      call_static_support "Emit_cmx"
        [Translquote.const source_file;
         Translquote.const prefix;
         Translquote.const module_name;
         Translquote.const (Load_path.get_paths ());
         Translquote.const flags;
         Translquote.const primitives;
         Translquote.const run_time.main_module_block_size;
         Translquote.const rt_required_globals;
         term]
    else
      call_static_support "Emit_cmo"
        [Translquote.const source_file;
         Translquote.const prefix;
         Translquote.const imports;
         Translquote.const flags;
         Translquote.const primitives;
         Translquote.const rt_required_globals;
         term]
    in
    List.fold_right
      (fun (h, code) body -> Llet (Strict, Pgenval, h, code, body))
      (List.rev !pending_splices) emit_call
  in

  let emit_macros =
    if quoted_builders () && exports_macros str then
      let mac = transl_macros_object module_name (str, cc) in
      let mac_primitives = !primitive_declarations in
      Some
        (call_static_support "Emit_cmo"
           [Translquote.const source_file;
            Translquote.const (prefix ^ "$macros");
            Translquote.const imports;
            Translquote.const flags;
            Translquote.const mac_primitives;
            Translquote.const mac.required_globals;
            call_static_support "Static_simplif"
              [Translquote.module_builder mac.code]])
    else None
  in
  let emit =
    match emit_macros with
    | None -> emit
    | Some em -> Lsequence (em, emit)
  in

  let static_id = Ident.create_persistent (module_name ^ "$static") in
  let code =
    protect_refs [R (Clflags.native_code, false)] @@ fun () ->
    at_compile_world @@ fun () ->
      transl_struct ~scopes Loc_unknown [] cc
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
  let code =
    static_report (Location.in_file source_file)
      "the compile-time part of this unit" code
  in
  { module_ident = static_id;
    main_module_block_size = 1;
    required_globals = required_globals ~flambda:false code;
    code }

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

let field_of_str loc str =
  let ids = Array.of_list (defined_idents str.str_items) in
  fun (pos, cc) ->
    match cc with
    | Tcoerce_primitive { pc_loc = _; pc_desc; pc_env; pc_type; } ->
        Translprim.transl_primitive loc pc_desc pc_env pc_type None
    | Tcoerce_alias (env, path, cc) ->
        let lam = transl_module_path loc env path in
        apply_coercion loc Alias cc lam
    | _ -> apply_coercion loc Strict cc (Lvar ids.(pos))


let transl_store_structure ~scopes glob map prims aliases str =
  let no_env_update _ _ env = env in
  let rec transl_store ~scopes rootpath subst cont = function
    [] ->
      transl_store_subst := subst;
      Lambda.subst no_env_update subst cont
    | item :: rem ->
        match item.str_desc with
        | Tstr_eval (expr, _attrs) ->
            Lsequence(Lambda.subst no_env_update subst
                        (transl_exp ~scopes expr),
                      transl_store ~scopes rootpath subst cont rem)
        | Tstr_value(rec_flag, st_lev, pat_expr_list)
          when st_lev < 0 && not (Translcore.in_toplevel ()) ->
            let ids = List.map macro_bound_id pat_expr_list in
            let lam =
              macro_env_slots rec_flag pat_expr_list
                (store_idents Loc_unknown ids)
            in
            Lsequence(Lambda.subst no_env_update subst lam,
                      transl_store ~scopes rootpath
                        (add_idents false ids subst) cont rem)
        | Tstr_value(rec_flag, _, pat_expr_list) ->
            let ids = let_bound_idents pat_expr_list in
            let lam =
              transl_let ~scopes ~in_structure:true rec_flag pat_expr_list
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
              transl_type_extension ~scopes item.str_env rootpath tyext
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
        | Tstr_module ({mb_presence=Mp_present} as mb)
          when not (Translcore.in_toplevel ())
               && template_functor_parts mb.mb_expr <> None ->
            let params, fbody = peel_template_params mb.mb_expr in
            let subscopes = match mb.mb_id with
              | None -> scopes
              | Some id -> enter_module_definition ~scopes id in
            let tt =
              transl_template_body ~scopes:subscopes ~loc:mb.mb_loc
                params fbody
            in
            let value =
              macro_env_of_roots tt.tt_roots
            in
            begin match mb.mb_id with
            | None ->
                Lsequence
                  (Lprim (Pignore,
                          [Lambda.subst no_env_update subst value],
                          of_location ~scopes mb.mb_name.loc),
                   transl_store ~scopes rootpath subst cont rem)
            | Some id ->
                Llet (Strict, Pgenval, id,
                      Lambda.subst no_env_update subst value,
                      Lsequence
                        (store_ident (of_location ~scopes mb.mb_loc) id,
                         transl_store ~scopes rootpath
                           (add_ident true id subst) cont rem))
            end
        | Tstr_module ({mb_presence=Mp_present} as mb)
          when not (Translcore.in_toplevel ())
               && template_application_parts mb.mb_expr <> None ->
            let m_id, funct, argopt = template_application_checks mb in
            let mcode = template_application_slot m_id in
            bind_literal_template_functor ~scopes
              ~transform:(Lambda.subst no_env_update subst) ~loc:mb.mb_loc
              m_id funct
              (bind_literal_template_argument ~scopes
                 ~transform:(Lambda.subst no_env_update subst) m_id argopt
                 (Llet (Strict, Pgenval, m_id,
                        apply_coercion (of_location ~scopes mb.mb_loc)
                          Strict (template_application_rescc mb.mb_expr)
                          (Lsplice (Lvar mcode)),
                        Lsequence
                          (store_ident (of_location ~scopes mb.mb_loc) m_id,
                           transl_store ~scopes rootpath
                             (add_ident true m_id subst) cont rem))))
        | Tstr_include incl
          when not (Translcore.in_toplevel ())
               && template_application_parts incl.incl_mod <> None ->
            let funct, argopt =
              Option.get (template_application_parts incl.incl_mod)
            in
            let loc = incl.incl_loc in
            template_application_mode_check ~loc;
            template_application_shape_checks ~loc funct;
            let m_id = template_include_binder loc in
            let mcode = template_application_slot m_id in
            let ids = bound_value_identifiers incl.incl_type in
            let sloc = of_location ~scopes loc in
            let rec store_incl_idents pos = function
              | [] -> transl_store ~scopes rootpath
                        (add_idents true ids subst) cont rem
              | id :: idl ->
                  Llet (Alias, Pgenval, id,
                        Lprim (Pfield (pos, Pointer, Mutable), [Lvar m_id],
                               sloc),
                        Lsequence (store_ident sloc id,
                                   store_incl_idents (pos + 1) idl))
            in
            bind_literal_template_functor ~scopes
              ~transform:(Lambda.subst no_env_update subst) ~loc m_id funct
              (bind_literal_template_argument ~scopes
                 ~transform:(Lambda.subst no_env_update subst) m_id argopt
                 (Llet (Strict, Pgenval, m_id, Lsplice (Lvar mcode),
                        store_incl_idents 0 ids)))
        | Tstr_module
            {mb_id=None; mb_name; mb_presence=Mp_present; mb_expr=modl;
             mb_loc=loc; mb_attributes} ->
            let lam =
              Translattribute.add_inline_attribute
                (transl_module ~scopes Tcoerce_none None modl)
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
            let field = field_of_str loc str in
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
                   ~scopes:(enter_module_definition ~scopes id)
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
            compile_recmodule ~scopes
              (fun id modl ->
                 Lambda.subst no_env_update subst
                   (match id with
                    | None ->
                      transl_module ~scopes Tcoerce_none None modl
                    | Some id ->
                      transl_module
                        ~scopes:(enter_module_definition ~scopes id)
                        Tcoerce_none (field_path rootpath id) modl))
              bindings
              (Lsequence(store_idents Loc_unknown ids,
                         transl_store ~scopes rootpath
                           (add_idents true ids subst) cont rem))
        | Tstr_class cl_list ->
            let (ids, class_bindings) = transl_class_bindings ~scopes cl_list in
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
            let field = field_of_str (of_location ~scopes loc) str in
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
                   (transl_module ~scopes Tcoerce_none None modl),
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
                        (transl_module ~scopes Tcoerce_none None od.open_expr),
                      store_idents 0 ids)
          end
        | Tstr_modtype _
        | Tstr_class_type _
        | Tstr_attribute _ ->
            transl_store ~scopes rootpath subst cont rem

  and store_ident loc id =
    try
      let (pos, cc) = Ident.find_same id map in
      let init_val = apply_coercion loc Alias cc (Lvar id) in
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
    let path_lam = transl_module_path Loc_unknown env path in
    let init_val = apply_coercion Loc_unknown Strict cc path_lam in
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

let () =
  store_ident_positions_fwd :=
    (fun ({ str_items = str; _ }, restr) ->
       let (map, _prims, _aliases, _size) =
         build_ident_map restr (defined_idents str) (more_idents str)
       in
       fun id ->
         match Ident.find_same id map with
         | (pos, Tcoerce_none) -> Some pos
         | _ -> None
         | exception Not_found -> None)

(* Compile an implementation using transl_store_structure
   (for the native-code compiler). *)

let transl_store_gen ~scopes module_name ({ str_items = str }, restr) topl =
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
          (transl_exp ~scopes expr)
    | str -> transl_store_structure ~scopes module_id map prims aliases str
  in
  transl_store_label_init module_id size f str
  (*size, transl_label_init (transl_store_structure module_id map prims str)*)

let transl_store_phrases module_name str =
  let scopes =
    enter_module_definition ~scopes:empty_scopes
      (Ident.create_persistent module_name)
  in
  transl_store_gen ~scopes module_name (str,Tcoerce_none) true

let transl_store_implementation module_name (str, restr) =
  let s = !transl_store_subst in
  transl_store_subst := Ident.Map.empty;
  let module_ident = Ident.create_persistent module_name in
  let scopes = enter_module_definition ~scopes:empty_scopes module_ident in
  let (i, code) = transl_store_gen ~scopes module_name (str, restr) false in
  transl_store_subst := s;
  { Lambda.main_module_block_size = i;
    code;
    (* module_ident is not used by closure, but this allow to share
       the type with the flambda version *)
    module_ident;
    required_globals = required_globals ~flambda:true code }

let () = transl_store_implementation_fwd := transl_store_implementation

(* Compile a toplevel phrase *)

let translated_at_current_stage level = in_run_world () || level < 0

let transl_toplevel_template_macro loc env path =
  let rec reroot root_lam = function
    | Env.Aident _ -> root_lam
    | Env.Adot (a, pos) ->
        Lprim (Pfield (pos, Pointer, Immutable), [reroot root_lam a],
               Loc_unknown)
  in
  let addr = Env.find_value_address path env in
  let root = address_root addr in
  let func = reroot (toploop_getvalue_key (toplevel_record_key root)) addr in
  let env_arg =
    Translquote.quote_access (transl_value_env_path Loc_unknown env path)
  in
  Lapply { ap_func = func; ap_args = [ env_arg ]; ap_loc = loc;
           ap_tailcall = Default_tailcall; ap_inlined = Default_inline;
           ap_specialised = Default_specialise }

let () =
  Translcore.toplevel_template_macro_transl := transl_toplevel_template_macro

let toplevel_app_hole_idents : (Ident.t, Ident.t) Hashtbl.t =
  Hashtbl.create 8

let toplevel_app_hole m_id =
  match Hashtbl.find_opt toplevel_app_hole_idents m_id with
  | Some h -> h
  | None ->
      let h = Ident.create_local "*tapphole*" in
      Hashtbl.add toplevel_app_hole_idents m_id h;
      h

let toplevel_template_application_hole m_id =
  Lapply { ap_func = Lvar (toplevel_app_hole m_id);
           ap_args = [ lambda_unit ];
           ap_loc = Loc_unknown; ap_tailcall = Default_tailcall;
           ap_inlined = Default_inline;
           ap_specialised = Default_specialise }

let transl_toplevel_template_application ?cont ~scopes mb =
  let m_id, funct, argopt =
    template_application_shape mb
  in
  let lit_funct =
    if is_module_path funct then None
    else Some (template_application_idents m_id).ta_funct
  in
  let lit_arg =
    match argopt with
    | Some (arg, _) when not (is_module_path arg) ->
        Some (template_application_idents m_id).ta_arg
    | _ -> None
  in
  let bind_literals inner =
    bind_literal_template_functor ~scopes ~loc:mb.Typedtree.mb_loc m_id
      funct
      (bind_literal_template_argument ~scopes m_id argopt inner)
  in
  set_toplevel_unique_name m_id;
  Translcore.register_toplevel_template_module m_id;
  if in_run_world () then
    bind_literals
      (toploop_setvalue m_id
         (apply_coercion Loc_unknown Strict
            (template_application_rescc mb.Typedtree.mb_expr)
            (Lsplice (toplevel_template_application_hole m_id))))
  else
    let cont = match cont with Some k -> k | None -> fun () -> lambda_unit in
    let pr = Ident.create_local "*tpair*" in
    let mc = Ident.create_local "*tappcode*" in
    let field i =
      Lprim (Pfield (i, Pointer, Immutable), [Lvar pr], Loc_unknown)
    in
    bind_literals @@
    Llet (Strict, Pgenval, pr,
          template_component_call ~scopes ~loc:mb.Typedtree.mb_loc
            ?lit_funct ?lit_arg m_id funct argopt,
      Lsequence (
        toploop_setvalue_key (toplevel_record_key m_id)
          (apply_coercion Loc_unknown Strict
             (template_application_rescc mb.Typedtree.mb_expr) (field 0)),
        Llet (Strict, Pgenval, mc,
              Lapply { ap_func = field 1;
                       ap_args =
                         [ template_env_code ~scopes
                             ~loc:mb.Typedtree.mb_loc m_id funct argopt ];
                       ap_loc = Loc_unknown;
                       ap_tailcall = Default_tailcall;
                       ap_inlined = Default_inline;
                       ap_specialised = Default_specialise },
              Llet (Strict, Pgenval, toplevel_app_hole m_id,
                    lfunction ~kind:Curried
                      ~params:[ (Ident.create_local "*unit*", Pgenval) ]
                      ~return:Pgenval ~body:(Lvar mc)
                      ~attr:default_function_attribute ~loc:Loc_unknown,
                    cont ()))))

let bound_components sg =
  List.filter_map
    (function
      | (Sig_value (id, {val_kind = Val_reg; _}, _)
        | Sig_typext (id, _, _, _)
        | Sig_module (id, Mp_present, _, _, _)
        | Sig_class (id, _, _, _)) as item -> Some (id, item)
      | _ -> None)
    sg

let set_at_compile_time_stage = function
  | Sig_value (_, vd, _) -> vd.val_staging_level < 0
  | Sig_module _ -> true
  | _ -> false

let store_components ~block comps k =
  let rec go pos = function
    | [] -> k ()
    | (id, sg_item) :: rest ->
        if in_compile_world () && not (set_at_compile_time_stage sg_item)
        then go (pos + 1) rest
        else
          Lsequence (toploop_setvalue id
                       (Lprim (Pfield (pos, Pointer, Mutable),
                               [Lvar block], Loc_unknown)),
                     go (pos + 1) rest)
  in
  go 0 comps

let transl_toplevel_item ~scopes item =
  match item.str_desc with
    Tstr_eval (expr, _) when in_run_world () -> transl_exp ~scopes expr
  | Tstr_value(Nonrecursive, level,
               [{vb_pat = {pat_desc=Tpat_any};vb_expr = expr}])
    when translated_at_current_stage level ->
      (* special compilation for toplevel "let _ = expr", so
         that Toploop can display the result of the expression.
         Otherwise, the normal compilation would result
         in a Lsequence returning unit. *)
      transl_exp ~scopes expr
  | Tstr_eval _ -> lambda_unit
  | Tstr_value(rec_flag, level, pat_expr_list)
    when translated_at_current_stage level ->
      let idents = let_bound_idents pat_expr_list in
      let go () =
        transl_let ~scopes ~in_structure:true rec_flag pat_expr_list
          (make_sequence toploop_setvalue_id idents)
      in
      if level >= 0 then go () else begin
      List.iter set_toplevel_unique_name idents;
      toplevel_macro_pairs ~scopes rec_flag pat_expr_list @@ fun pairs ->
      make_sequence
        (fun (id, pair) -> toploop_setvalue id pair)
        pairs
      end
  | Tstr_value _ -> lambda_unit
  | Tstr_typext(tyext) ->
      let idents =
        List.map (fun ext -> ext.ext_id) tyext.tyext_constructors
      in
      (* we need to use unique name in case of multiple
         definitions of the same extension constructor in the toplevel *)
      List.iter set_toplevel_unique_name idents;
        transl_type_extension ~scopes item.str_env None tyext
          (make_sequence toploop_setvalue_id idents)
  | Tstr_exception ext ->
      set_toplevel_unique_name ext.tyexn_constructor.ext_id;
      toploop_setvalue ext.tyexn_constructor.ext_id
        (transl_extension_constructor ~scopes
           item.str_env None ext.tyexn_constructor)
  | Tstr_module ({mb_id = Some id; mb_presence = Mp_present} as mb)
    when template_functor_parts mb.mb_expr <> None ->
      let params, fbody = peel_template_params mb.mb_expr in
      set_toplevel_unique_name id;
      register_toplevel_template_functor id;
      let tt =
        transl_template_body ~scopes:(enter_module_definition ~scopes id)
          ~loc:mb.mb_loc params fbody
      in
      let store_component =
        toploop_setvalue_key (template_component_key id) tt.tt_function
      in
      if in_compile_world () then store_component
      else
        let env_tuple =
          macro_env_of_roots tt.tt_roots
        in
        Lsequence (toploop_setvalue id env_tuple, store_component)
  | Tstr_module ({mb_presence = Mp_present} as mb)
    when template_application_parts mb.mb_expr <> None ->
      transl_toplevel_template_application ~scopes mb
  | Tstr_module ({mb_id = Some id; mb_presence = Mp_present;
                  mb_expr } )
    when (match mb_expr.mod_desc with
          | Tmod_functor (Plain, _, _) | Tmod_ident _ -> true
          | _ -> false)
         && Mtype.contains_mixed_functor ~deep_templates:false
              mb_expr.mod_env mb_expr.mod_type ->
      set_toplevel_unique_name id;
      register_toplevel_mixed_functor id;
      if in_compile_world () then
        toploop_setvalue_key (toplevel_mixed_key id)
          (transl_module ~scopes Tcoerce_none None mb_expr)
      else
        Lsequence
          (toploop_setvalue id
             (transl_module
                ~scopes:(enter_module_definition ~scopes id)
                Tcoerce_none (Some (Pident id)) mb_expr),
           toploop_setvalue_key (toplevel_mixed_key id)
             (at_compile_world (fun () ->
                  transl_module ~scopes Tcoerce_none None mb_expr)))
  | Tstr_module ({mb_id = Some id; mb_presence = Mp_present; mb_expr})
    when (match mb_expr.mod_desc with
          | Tmod_apply (_, funct, _, _) | Tmod_apply_unit (_, funct) ->
              Mtype.contains_mixed_functor ~deep_templates:false
                funct.mod_env funct.mod_type
              && (module_expr_path funct <> None
                  || is_plain_functor funct)
          | _ -> false)
         && Mtype.has_macro_components mb_expr.mod_env mb_expr.mod_type ->
      set_toplevel_unique_name id;
      Translcore.register_toplevel_template_module id;
      if in_compile_world () then
        toploop_setvalue_key (toplevel_record_key id)
          (transl_module ~scopes Tcoerce_none None mb_expr)
      else
        Lsequence
          (toploop_setvalue id
             (transl_module
                ~scopes:(enter_module_definition ~scopes id)
                Tcoerce_none (Some (Pident id)) mb_expr),
           toploop_setvalue_key (toplevel_record_key id)
             (at_compile_world (fun () ->
                  transl_module ~scopes Tcoerce_none None mb_expr)))
  | Tstr_module ({mb_id = Some id; mb_presence = Mp_present; mb_expr})
    when (match mb_expr.mod_desc with
          | Tmod_structure _ -> true
          | Tmod_constraint ({mod_desc = Tmod_structure _; _}, _, _, _) ->
              true
          | _ -> false)
         && (module_has_template_definition mb_expr
             || Mtype.contains_mixed_functor ~deep_templates:false
                  mb_expr.mod_env mb_expr.mod_type) ->
      set_toplevel_unique_name id;
      Translcore.register_toplevel_template_module id;
      with_mixed_body_batch_idents mb_expr @@ fun () ->
      if in_compile_world () then
        toploop_setvalue_key (toplevel_record_key id)
          (transl_module ~scopes:(enter_module_definition ~scopes id)
             Tcoerce_none None mb_expr)
      else
        Lsequence
          (toploop_setvalue id
             (transl_module ~scopes:(enter_module_definition ~scopes id)
                Tcoerce_none (Some (Pident id)) mb_expr),
           toploop_setvalue_key (toplevel_record_key id)
             (at_compile_world (fun () ->
                  transl_module
                    ~scopes:(enter_module_definition ~scopes id)
                    Tcoerce_none None mb_expr)))
  | Tstr_module {mb_id=None; mb_presence=Mp_present; mb_expr=modl} ->
      transl_module ~scopes Tcoerce_none None modl
  | Tstr_module {mb_id=Some id; mb_presence=Mp_present; mb_expr=modl} ->
      (* we need to use the unique name for the module because of issues
         with "open" (PR#8133) *)
      set_toplevel_unique_name id;
      let lam = transl_module
                  ~scopes:(enter_module_definition ~scopes id)
                  Tcoerce_none (Some(Pident id)) modl in
      toploop_setvalue id lam
  | Tstr_recmodule bindings ->
      let idents = List.filter_map (fun mb -> mb.mb_id) bindings in
      compile_recmodule ~scopes
        (fun id modl ->
           match id with
           | None ->
             transl_module ~scopes Tcoerce_none None modl
           | Some id ->
             transl_module
               ~scopes:(enter_module_definition ~scopes id)
               Tcoerce_none (Some (Pident id)) modl)
        bindings
        (make_sequence toploop_setvalue_id idents)
  | Tstr_class _ when in_compile_world () ->
      lambda_unit
  | Tstr_class cl_list ->
      (* we need to use unique names for the classes because there might
         be a value named identically *)
      let (ids, class_bindings) = transl_class_bindings ~scopes cl_list in
      List.iter set_toplevel_unique_name ids;
      Value_rec_compiler.compile_letrec class_bindings
        (make_sequence toploop_setvalue_id ids)
  | Tstr_include incl ->
      let comps = bound_components incl.incl_type in
      let modl = incl.incl_mod in
      let is_macro_comp = function
        | Sig_value (_, vd, _) -> vd.val_staging_level < 0
        | _ -> false
      in
      let is_macro_module_comp = function
        | Sig_module (_, _, md, _, _) ->
            Mtype.has_macro_components modl.mod_env md.md_type
        | _ -> false
      in
      let batch_shaped =
        List.exists
          (fun (_, it) -> is_macro_comp it || is_macro_module_comp it)
          comps
        && (match module_expr_path modl with
            | Some p ->
                let p =
                  try Env.normalize_module_path None modl.mod_env p
                  with Not_found -> p
                in
                let rec root (p : Path.t) =
                  match p with
                  | Pident id -> Some id
                  | Pdot (p, _) | Pextra_ty (p, _) -> root p
                  | Papply _ -> None
                in
                (match root p with
                 | Some id ->
                     Ident.global id
                     || Translcore.is_toplevel_template_module id
                 | None -> false)
            | None ->
                (match modl.mod_desc with
                 | Tmod_apply (_, funct, _, _)
                 | Tmod_apply_unit (_, funct) ->
                     Mtype.contains_mixed_functor ~deep_templates:false
                       funct.mod_env funct.mod_type
                     && (module_expr_path funct <> None
                         || is_plain_functor funct)
                 | _ -> false))
      in
      if not batch_shaped then
        let mid = Ident.create_local "include" in
        Llet(Strict, Pgenval, mid,
             transl_module ~scopes Tcoerce_none None modl,
             store_components ~block:mid comps (fun () -> lambda_unit))
      else
        let field v pos =
          Lprim (Pfield (pos, Pointer, Mutable), [Lvar v], Loc_unknown)
        in
        let pair fn env =
          Lprim (Pmakeblock (0, Immutable, None), [fn; env], Loc_unknown)
        in
        if in_compile_world () then
          let mid_s = Ident.create_local "include$static" in
          let rec set_idents pos = function
            | [] -> lambda_unit
            | (id, sg_item) :: rest ->
                if not (set_at_compile_time_stage sg_item)
                then set_idents (pos + 1) rest
                else if is_macro_module_comp sg_item then begin
                  Translcore.register_toplevel_template_module id;
                  Lsequence
                    (toploop_setvalue_key (toplevel_record_key id)
                       (field mid_s pos),
                     set_idents (pos + 1) rest)
                end else
                  Lsequence
                    (toploop_setvalue id
                       (if is_macro_comp sg_item
                        then pair (field mid_s pos) (Lconst const_unit)
                        else field mid_s pos),
                     set_idents (pos + 1) rest)
          in
          Llet (Strict, Pgenval, mid_s,
                transl_module ~scopes Tcoerce_none None modl,
                set_idents 0 comps)
        else
          let mid = Ident.create_local "include" in
          let mid_s = Ident.create_local "include$static" in
          let rec set_idents pos = function
            | [] -> lambda_unit
            | (id, sg_item) :: rest ->
                if is_macro_module_comp sg_item then begin
                  Translcore.register_toplevel_template_module id;
                  Lsequence
                    (toploop_setvalue id (field mid pos),
                     Lsequence
                       (toploop_setvalue_key (toplevel_record_key id)
                          (field mid_s pos),
                        set_idents (pos + 1) rest))
                end else
                Lsequence
                  (toploop_setvalue id
                     (if is_macro_comp sg_item
                      then pair (field mid_s pos) (field mid pos)
                      else field mid pos),
                   set_idents (pos + 1) rest)
          in
          Llet (Strict, Pgenval, mid,
                transl_module ~scopes Tcoerce_none None modl,
            Llet (Strict, Pgenval, mid_s,
                  at_compile_world (fun () ->
                      transl_module ~scopes Tcoerce_none None modl),
                  set_idents 0 comps))
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
          let comps = bound_components od.open_bound_items in
          let mid = Ident.create_local "open" in
          Llet(pure, Pgenval, mid,
               transl_module ~scopes Tcoerce_none None od.open_expr,
               store_components ~block:mid comps (fun () -> lambda_unit))
      end
  | Tstr_module ({mb_presence=Mp_absent}) ->
      lambda_unit
  | Tstr_modtype _
  | Tstr_type _
  | Tstr_class_type _
  | Tstr_attribute _ ->
      lambda_unit

let transl_toplevel_item_and_close ?except ~scopes itm =
  close_toplevel_term ?except
    (transl_label_init (fun () -> transl_toplevel_item ~scopes itm))

let module_has_toplevel_splice (me : Typedtree.module_expr) =
  let found = ref false in
  let expr iter (e : Typedtree.expression) =
    (match e.exp_desc with
     | Texp_splice { spl_index = Some _; _ } -> found := true
     | _ -> ());
    Tast_iterator.default_iterator.expr iter e
  in
  let module_expr iter (me : Typedtree.module_expr) =
    match me.mod_desc with
    | Tmod_functor (Template, _, _) -> ()
    | _ -> Tast_iterator.default_iterator.module_expr iter me
  in
  let iter = { Tast_iterator.default_iterator with expr; module_expr } in
  iter.module_expr iter me;
  !found

let check_toplevel_phrase_shapes (str : Typedtree.structure) =
  let record_keyed (me : Typedtree.module_expr) =
    let rec path_root (me : Typedtree.module_expr) =
      match me.mod_desc with
      | Tmod_ident (p, _) -> Some (Path.head p)
      | Tmod_constraint (m, _, _, _) -> path_root m
      | _ -> None
    in
    match path_root me with
    | Some id -> Translcore.is_toplevel_template_module id
    | None -> false
  in
  let rec check_binding (me : Typedtree.module_expr) =
    match me.mod_desc with
    | Tmod_ident _ -> ()
    | Tmod_constraint (inner, _, _, _) when record_keyed inner ->
        raise (Error (me.mod_loc, Toplevel_macro_module_rebinding))
    | Tmod_constraint (inner, _, _, _) -> check_binding inner
    | Tmod_structure str ->
        List.iter
          (fun (it : Typedtree.structure_item) ->
             match it.str_desc with
             | Tstr_include incl when record_keyed incl.incl_mod ->
                 raise (Error (incl.incl_loc,
                               Toplevel_macro_module_rebinding))
             | Tstr_module { mb_expr; _ } -> check_binding mb_expr
             | _ -> ())
          str.str_items
    | _ -> ()
  in
  List.iter
    (fun (it : Typedtree.structure_item) ->
       match it.str_desc with
       | Tstr_module { mb_expr; _ } -> check_binding mb_expr
       | Tstr_recmodule bindings ->
           List.iter
             (fun (mb : Typedtree.module_binding) ->
                if module_has_toplevel_splice mb.mb_expr then
                  raise (Error (mb.mb_expr.mod_loc,
                                Toplevel_splice_in_recmodule)))
             bindings
       | _ -> ())
    str.str_items

let transl_toplevel_definition str =
  Translcore.with_toplevel_mode @@ fun () ->
  check_toplevel_phrase_shapes str;
  reset_labels ();
  Translprim.clear_used_primitives ();
  make_sequence
    (transl_toplevel_item_and_close ~scopes:empty_scopes)
    str.str_items

let toplevel_has_template_applications str =
  List.exists
    (fun item ->
       match item.Typedtree.str_desc with
       | Tstr_module mb -> template_application_parts mb.mb_expr <> None
       | _ -> false)
    str.str_items

let reset_toplevel_phrase () =
  TranslSplices.reset ();
  Hashtbl.reset toplevel_splice_hole_idents;
  Hashtbl.reset toplevel_app_hole_idents

let transl_toplevel_phrase_static ~run_term str =
  Translcore.with_toplevel_mode @@ fun () ->
  let scopes = empty_scopes in
  let close_around_cont build rest_lam =
    let k = Ident.create_local "*phrase-cont*" in
    let lam = build (fun () -> Lvar k) in
    let closed =
      close_toplevel_term ~except:(Ident.Set.singleton k) lam
    in
    Lambda.subst (fun _ _ env -> env)
      (Ident.Map.singleton k rest_lam) closed
  in
  at_compile_world @@ fun () ->
  let rec chain items =
    match items with
    | [] ->
        Translquote.quote_module_lambda run_term
    | item :: rest ->
        let splices =
          List.map
            (fun (idx, lam) -> (idx, close_toplevel_term lam))
            (TranslSplices.transl_item_splices ~scopes item)
        in
        let body =
          match item.Typedtree.str_desc with
          | Tstr_module ({ mb_presence = Mp_present } as mb)
            when template_application_parts mb.mb_expr <> None ->
              close_around_cont
                (fun k ->
                   transl_toplevel_template_application ~cont:k ~scopes
                     mb)
                (chain rest)
          | Tstr_module ({ mb_presence = Mp_present; mb_expr; _ } as mb)
            when module_has_toplevel_splice mb_expr ->
              close_around_cont
                (fun k ->
                   let store block =
                     match mb.mb_id with
                     | Some id ->
                         set_toplevel_unique_name id;
                         Lsequence (toploop_setvalue id block, k ())
                     | None ->
                         Lsequence
                           (Lprim (Pignore, [block], Loc_unknown), k ())
                   in
                   transl_module_hoisted ~scopes Tcoerce_none None
                     mb_expr store)
                (chain rest)
          | Tstr_include incl
            when module_has_toplevel_splice incl.incl_mod ->
              close_around_cont
                (fun k ->
                   let comps = bound_components incl.incl_type in
                   let mid = Ident.create_local "include" in
                   transl_module_hoisted ~scopes Tcoerce_none None
                     incl.incl_mod
                     (fun block ->
                        Llet (Strict, Pgenval, mid, block,
                              store_components ~block:mid comps k)))
                (chain rest)
          | Tstr_open od
            when module_has_toplevel_splice od.open_expr ->
              close_around_cont
                (fun k ->
                   let comps = bound_components od.open_bound_items in
                   let mid = Ident.create_local "open" in
                   transl_module_hoisted ~scopes Tcoerce_none None
                     od.open_expr
                     (fun block ->
                        Llet (Strict, Pgenval, mid, block,
                              store_components ~block:mid comps k)))
                (chain rest)
          | _ ->
              Lsequence
                (transl_toplevel_item_and_close ~scopes item, chain rest)
        in
        insert_item_splices splices body
  in
  chain str.str_items

let toplevel_splice_hole idx spl_exp =
  let roots =
    Ident.Set.elements
      (Ident.Set.filter (fun r -> not (Ident.global r))
         (Translquote.fv spl_exp))
  in
  Lapply { ap_func = Lvar (toplevel_splice_hole_ident idx);
           ap_args = List.map (fun r -> Lvar r) roots @ [ lambda_unit ];
           ap_loc = Loc_unknown;
           ap_tailcall = Default_tailcall;
           ap_inlined = Default_inline;
           ap_specialised = Default_specialise }


let get_component = function
    None -> Lconst const_unit
  | Some id -> Lprim(Pgetglobal id, [], Loc_unknown)

let transl_package_flambda component_names coercion =
  module_block_size component_names coercion,
  apply_coercion Loc_unknown Strict coercion
    (Lprim(Pmakeblock(0, Immutable, None),
           List.map get_component component_names,
           Loc_unknown))

let transl_package component_names target_name coercion =
  let components =
    Lprim(Pmakeblock(0, Immutable, None),
          List.map get_component component_names, Loc_unknown) in
  Lprim(Psetglobal target_name,
        [apply_coercion Loc_unknown Strict coercion components],
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
          (fun (pos, cc) -> apply_coercion Strict cc (get_component g.(pos)))
          pos_cc_list
    | _ ->
        assert false in
  Lprim(Psetglobal target_name, [Lprim(Pmakeblock(0, Immutable), components)])
   *)

let transl_store_package component_names target_name coercion =
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
             apply_coercion Loc_unknown Strict coercion components,
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
                  apply_coercion Strict cc (get_component id.(src))]))
         0 pos_cc_list)
  *)
  | _ -> assert false

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
  | Template_functor_not_supported ->
      Location.errorf ~loc
        "Template functors are not yet supported here."
  | Template_restriction msg ->
      Location.errorf ~loc "%a" Format_doc.pp_print_text msg
  | Toplevel_macro_module_rebinding ->
      Location.errorf ~loc
        "This rebinds a module whose macros or template functors are held\
       @ under the name it was bound to, which a constraint or an include\
       @ inside a structure cannot carry across.\
       @ Bind it with a plain alias (%a), or refer to its members\
       @ through their original path."
        Style.inline_code "module B = A"
  | Toplevel_splice_in_recmodule ->
      Location.errorf ~loc
        "A top-level splice is not yet supported inside a recursive module\
       @ binding in the toplevel.  Compiling the same code with %a works;\
       @ so does binding the spliced value outside the recursive group."
        Style.inline_code "ocamlc"

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
  Env.reset_required_globals ();
  Translprim.clear_used_primitives ();
  reset_template_applications ();
  Hashtbl.reset anonymous_application_ids;
  template_include_binders := [];
  toplevel_template_functors := Ident.Set.empty;
  Hashtbl.reset toplevel_splice_hole_idents;
  Hashtbl.reset toplevel_app_hole_idents;
  Translcore.reset_toplevel_template_modules ()
