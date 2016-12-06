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
open Longident
open Path
open Types
open Typedtree
open Lambda
open Translobj
open Translcore
open Translclass

type error =
  Circular_dependency of Ident.t


exception Error of Location.t * error

(* record macros and cross-stage identifiers in a tree *)

module TreeInspectIt (X : sig
  val cs_glob : Ident.t loc list ref
  val cs_nonglob : Path.t list ref
  val macros : Path.t list ref
end) = struct
  open TypedtreeIter
  include DefaultIteratorArgument

  let enter_expression exp =
    match exp.exp_desc with
    | Texp_ident (p, lid, vd) ->
        let h = Path.head p in
        if Env.find_stage p exp.exp_env <> Env.cur_stage exp.exp_env then
          if Ident.global h || Ident.persistent h then
            let h' = { txt = h; loc = lid.loc } in
            X.cs_glob := h' :: !X.cs_glob
          else if not (List.mem p !X.cs_nonglob) then
            X.cs_nonglob := p :: !X.cs_nonglob
        ;
        if vd.val_kind = Val_macro &&
            not (List.mem p !X.macros) then
          X.macros := p :: !X.macros
    | _ -> ()
end

module TreeInspect : sig
  val expression : expression -> Ident.t loc list * Path.t list * Path.t list
end = struct
  module X = struct
    let cs_glob = ref ([] : Ident.t loc list)
    let cs_nonglob = ref ([] : Path.t list)
    let macros = ref ([] : Path.t list)
    let reset () =
      cs_glob := [];
      cs_nonglob := [];
      macros := []
  end
  include TypedtreeIter.MakeIterator(TreeInspectIt(X))

  let expression exp =
    iter_expression exp;
    let ret = (!X.cs_glob, !X.cs_nonglob, !X.macros) in
    X.reset ();
    ret
end

let transl_toplevel_splice exp =
  let (cs_glob, _, _) = TreeInspect.expression exp in
  List.fold_left
    (fun lam id -> Translquote.wrap_local exp.exp_loc id.txt
      (Location.mkloc (Ident.name id.txt) (id : Ident.t loc).loc) lam)
    (Translcore.transl_exp exp)
    cs_glob

(* ordered array of lambda blocks for stage-0 splices *)
let item_splices = ref ([] : lambda list)

module TranslSplicesIterator = struct
  open TypedtreeIter
  include DefaultIteratorArgument

  let depth = ref 0

  let enter_expression expr =
    match expr.exp_desc with
    | Texp_escape e ->
        if !depth = 0 then
          let body = transl_toplevel_splice e in
          item_splices := !item_splices @ [body];
        else
          ()
    | Texp_quote _ ->
        incr depth
    | _ -> ()

  let leave_expression expr =
    match expr.exp_desc with
    | Texp_quote _ ->
        decr depth
    | _ -> ()
end

module TranslSplices = TypedtreeIter.MakeIterator(TranslSplicesIterator)

let splice_ids = ref ([] : Ident.t list)

let transl_item_splices item item_lam =
  (* Find splices in current structure item *)
  TranslSplices.iter_structure_item item;
  let item_splices_with_ids =
    List.map (fun l -> (Ident.create "*splice*", l)) !item_splices
  in
  (* Add the code for running splices *)
  let wrap_let str_lam (splice_id, splice_lam) =
    Llet(Strict, Pgenval, splice_id, splice_lam, str_lam)
  in
  let lam = List.fold_left wrap_let item_lam item_splices_with_ids in
  item_splices := [];
  splice_ids := !splice_ids @ List.map fst item_splices_with_ids;
  lam

(* Wrap a module construction lambda inside a lambda that declares the module
   global (as a side effect), constructs an array with all splices encountered
   in this module and returns it. [splice_ids] should refer to these splices.
   *)
let rec insert_splice_array module_id splice_ids = function
  | Llet (a,b,c,lam,rem) ->
      Llet (a, b, c, lam, insert_splice_array module_id splice_ids rem)
  | Lletrec (vbs, rem) ->
      Lletrec (vbs, insert_splice_array module_id splice_ids rem)
  | block ->
      let splice_body =
        Lprim (Pmakearray (Paddrarray, Mutable),
          List.map
            (fun id ->
              Translquote.transl_close_expression Location.none (Lvar id))
            splice_ids,
          Location.none)
      in
      Lsequence (
        Lprim (Psetglobal module_id, [block], Location.none),
        splice_body)

(* Wrap a piece of lambda code so that, if the original code returned a value,
 * the result of this function will execute the original code, marshal the
 * returned value to the file passed as the first command-line argument.
 * Intended for wrapping of splice-producing code in [ocaml*.opt]. *)
let wrap_marshal lam =
  let apply fun_lam args =
    Lapply {
      ap_func = fun_lam;
      ap_args = args;
      ap_loc = Location.none;
      ap_should_be_tailcall = false;
      ap_inlined = Default_inline;
      ap_specialised = Default_specialise;
    }
  in
  let lam_id = Ident.create "let" in
  let channel_id = Ident.create "channel" in
  let marshal_to_channel =
    Lprim (Pfield 0, (* ~Marshal.to_channel *)
      [Lprim (Pgetglobal (Ident.create_persistent
        (Ident.lift_string "Marshal")), [], Location.none)],
      Location.none)
  in
  let get_filename =
    (* ~Sys.argv.[1] *)
    Lprim (
      Pccall (
        Primitive.simple
          ~name:"caml_array_get" ~arity:2 ~alloc:true (* ? *)),
      [Lprim
        (Pfield 0, (* ~Sys.argv *)
          [Lprim
            (Pgetglobal (Ident.create_persistent (Ident.lift_string "Sys")),
            [], Location.none)],
        Location.none);
        (* 1 *)
        Lconst (Const_base (Const_int 1))
      ],
      Location.none)
  in
  let open_channel =
    apply
      (Lprim (Pfield 43, (* ~Pervasives.open_out_bin *)
        [Lprim (Pgetglobal (Ident.create_persistent 
          (Ident.lift_string "Pervasives")), [], Location.none)],
        Location.none))
      [get_filename]
  in
  let write_lam =
    Llet (Strict, Pgenval, channel_id, open_channel,
      Lsequence (
        apply
          marshal_to_channel
          [Lvar channel_id; Lvar lam_id; Lconst (Const_pointer 0)],
        apply
          (Lprim (Pfield 58, (* ~Pervasives.close_out *)
            [Lprim (Pgetglobal (Ident.create_persistent
              (Ident.lift_string "Pervasives")), [], Location.none)],
            Location.none))
          [Lvar channel_id]
      )
    )
  in
  Llet (Strict, Pgenval, lam_id, lam, write_lam)

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
  | Some p -> Some(Pdot(p, Ident.name field, Path.nopos))

(* Compile type extensions *)

let transl_type_extension env rootpath tyext body =
  List.fold_right
    (fun ext body ->
      let lam =
        transl_extension_constructor env (field_path rootpath ext.ext_id) ext
      in
      Llet(Strict, Pgenval, ext.ext_id, lam, body))
    tyext.tyext_constructors
    body

let get_field sf = function
  | Uniphase (sf', i) ->
      assert (sf = sf'); i
  | Biphase (i, j) ->
      if sf = Static then i else j

(* Compile a coercion *)

let zero_lam =
  Lconst (Const_base (Const_int 0))

let extract_phase_of_pos phase = function
  | Uniphase (sf, i) ->
      if sf = phase then Some i else None
  | Biphase (i, j) ->
      if phase = Static then Some i else Some j

(* Filter out positions for the other phase *)
let rec filter_pos_cc phase = function
  | [] -> []
  | (pos, cc) :: rem ->
    begin match extract_phase_of_pos phase pos with
    | None -> filter_pos_cc phase rem
    | Some i ->
        (i, cc) :: filter_pos_cc phase rem
    end

let rec filter_id_pos phase = function
  | [] -> []
  | (id, pos, cc) :: rem ->
    begin match extract_phase_of_pos phase pos with
    | None -> filter_id_pos phase rem
    | Some i ->
        (id, i, cc) :: filter_id_pos phase rem
    end

let rec apply_coercion loc static_flag strict restr arg =
  match restr with
    Tcoerce_none ->
      arg
  | Tcoerce_structure(pos_cc_list, id_pos_list) ->
      name_lambda strict arg (fun id ->
        let (pos_cc_list, id_pos_list) =
          (filter_pos_cc static_flag pos_cc_list,
            filter_id_pos static_flag id_pos_list)
        in
        let get_field pos =
          Lprim(Pfield pos,[Lvar id], loc)
        in
        let lam =
          Lprim(Pmakeblock(0, Immutable, None),
                List.map
                  (apply_coercion_field loc static_flag get_field)
                  pos_cc_list,
                loc)
        in
        wrap_id_pos_list loc static_flag id_pos_list get_field lam)
  | Tcoerce_functor(cc_arg, cc_res) ->
      let param = Ident.create "funarg" in
      name_lambda strict arg (fun id ->
        Lfunction{kind = Curried; params = [param];
                  attr = { default_function_attribute with
                           is_a_functor = true };
                  loc = loc;
                  body = apply_coercion
                           loc static_flag Strict cc_res
                           (Lapply{ap_should_be_tailcall=false;
                                   ap_loc=loc;
                                   ap_func=Lvar id;
                                   ap_args=[apply_coercion loc static_flag
                                     Alias cc_arg (Lvar param)];
                                   ap_inlined=Default_inline;
                                   ap_specialised=Default_specialise})})
  | Tcoerce_primitive { pc_loc; pc_desc; pc_env; pc_type; } ->
      if static_flag = Nonstatic then
        transl_primitive pc_loc pc_desc pc_env pc_type None
      else
        zero_lam
  | Tcoerce_alias (path, cc) ->
      name_lambda strict arg
        (fun _ -> apply_coercion loc static_flag Alias cc
          (transl_normal_path static_flag path))

and apply_coercion_field loc static_flag get_field (pos, cc) =
  apply_coercion loc static_flag Alias cc (get_field pos)

and wrap_id_pos_list loc static_flag id_pos_list get_field lam =
  let fv = free_variables lam in
  (*Format.eprintf "%a@." Printlambda.lambda lam;
  IdentSet.iter (fun id -> Format.eprintf "%a " Ident.print id) fv;
  Format.eprintf "@.";*)
  let (lam,s) =
    List.fold_left (fun (lam,s) (id',pos,c) ->
      if IdentSet.mem id' fv then
        let id'' = Ident.create (Ident.name id') in
        (Llet(Alias, Pgenval, id'',
              apply_coercion loc static_flag Alias c (get_field pos),lam),
         Ident.add id' (Lvar id'') s)
      else (lam,s))
      (lam, Ident.empty) id_pos_list
  in
  if s == Ident.empty then lam else subst_lambda s lam

(* Compose two coercions
   apply_coercion c1 (apply_coercion c2 e) behaves like
   apply_coercion (compose_coercions c1 c2) e. *)

let rec compose_coercions phase c1 c2 =
  match (c1, c2) with
    (Tcoerce_none, c2) -> c2
  | (c1, Tcoerce_none) -> c1
  | (Tcoerce_structure (pc1, ids1), Tcoerce_structure (pc2, ids2)) ->
      let v2 = Array.of_list pc2 in
      let ids1 =
        List.map (fun (id,pos1,c1) ->
            let (pos2,c2) = v2.(get_field phase pos1) in
            (id, pos2, compose_coercions phase c1 c2))
          ids1
      in
      Tcoerce_structure
        (List.map
          (function (p1, Tcoerce_primitive p) ->
                      (p1, Tcoerce_primitive p)
                  | (p1, c1) ->
                      let (p2, c2) = v2.(get_field phase p1) in
                      (p2, compose_coercions phase c1 c2))
             pc1,
         ids1 @ ids2)
  | (Tcoerce_functor(arg1, res1), Tcoerce_functor(arg2, res2)) ->
      Tcoerce_functor(compose_coercions phase arg2 arg1,
                      compose_coercions phase res1 res2)
  | (c1, Tcoerce_alias (path, c2)) ->
      Tcoerce_alias (path, compose_coercions phase c1 c2)
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

(* Record the primitive declarations occuring in the module compiled *)

let primitive_declarations = ref ([] : Primitive.description list)
let record_primitive = function
  | {val_kind=Val_prim p} ->
      primitive_declarations := p :: !primitive_declarations
  | _ -> ()

(* Utilities for compiling "module rec" definitions *)

let mod_prim phase name =
  let mod_name =
    if phase = Static then Ident.lift_string "CamlinternalMod"
    else "CamlinternalMod"
  in
  try
    transl_normal_path phase
      (fst (Env.lookup_value (Ldot (Lident mod_name, name))
                             Env.empty))
  with Not_found ->
    fatal_error ("Primitive " ^ name ^ " not found.")

let undefined_location loc =
  let (fname, line, char) = Location.get_pos_info loc.Location.loc_start in
  Lconst(Const_block(0,
                     [Const_base(Const_string (fname, None));
                      Const_base(Const_int line);
                      Const_base(Const_int char)]))

let init_shape target_phase modl =
  let rec init_shape_mod env mty =
    match Mtype.scrape env mty with
      Mty_ident _ ->
        raise Not_found
    | Mty_alias _ ->
        Const_block (1, [Const_pointer 0])
    | Mty_signature sg ->
        Const_block(0, [Const_block(0, init_shape_struct env sg)])
    | Mty_functor _ ->
        raise Not_found (* can we do better? *)
  and init_shape_struct env sg =
    match sg with
      [] -> []
    | Sig_value(_id, sf, {val_kind; val_type=ty}) :: rem 
          when val_kind = Val_macro ||
            (val_kind = Val_reg && sf = target_phase) ->
        let init_v =
          match Ctype.expand_head env ty with
            {desc = Tarrow(_,_,_,_)} ->
              Const_pointer 0 (* camlinternalMod.Function *)
          | {desc = Tconstr(p, _, _)} when Path.same p Predef.path_lazy_t ->
              Const_pointer 1 (* camlinternalMod.Lazy *)
          | _ -> raise Not_found in
        init_v :: init_shape_struct env rem
    | Sig_value(_, _, {val_kind=Val_prim _ | Val_reg}) :: rem ->
        init_shape_struct env rem
    | Sig_value _ :: _rem ->
        assert false
    | Sig_type(id, tdecl, _) :: rem ->
        init_shape_struct (Env.add_type ~check:false id tdecl env) rem
    | Sig_typext _ :: _ ->
        raise Not_found
    | Sig_module(id, md, sf, _) :: rem ->
        let phase = Env.cur_phase env + Env.phase_of_sf sf in
        if Env.contains_phase_mty target_phase env md.md_type then
          init_shape_mod env md.md_type ::
          init_shape_struct
            (Env.add_module_declaration ~check:false phase id md env) rem
        else
          init_shape_struct env rem
    | Sig_modtype(id, minfo) :: rem ->
        init_shape_struct (Env.add_modtype id minfo env) rem
    | Sig_class _ :: rem ->
        if target_phase = Static then
          init_shape_struct env rem
        else
          let x =
            Const_pointer 2 (* camlinternalMod.Class *)
          in
          x :: init_shape_struct env rem
    | Sig_class_type _ :: rem ->
        init_shape_struct env rem
  in
  try
    Some(undefined_location modl.mod_loc,
         Lconst(init_shape_mod modl.mod_env modl.mod_type))
  with Not_found ->
    None

(* Reorder bindings to honor dependencies.  *)

type binding_status = Undefined | Inprogress | Defined

let reorder_rec_bindings bindings =
  let id = Array.of_list (List.map (fun (id,_,_,_) -> id) bindings)
  and loc = Array.of_list (List.map (fun (_,loc,_,_) -> loc) bindings)
  and init = Array.of_list (List.map (fun (_,_,init,_) -> init) bindings)
  and rhs = Array.of_list (List.map (fun (_,_,_,rhs) -> rhs) bindings) in
  let fv = Array.map Lambda.free_variables rhs in
  let num_bindings = Array.length id in
  let status = Array.make num_bindings Undefined in
  let res = ref [] in
  let rec emit_binding i =
    match status.(i) with
      Defined -> ()
    | Inprogress -> raise(Error(loc.(i), Circular_dependency id.(i)))
    | Undefined ->
        if init.(i) = None then begin
          status.(i) <- Inprogress;
          for j = 0 to num_bindings - 1 do
            if IdentSet.mem id.(j) fv.(i) then emit_binding j
          done
        end;
        res := (id.(i), init.(i), rhs.(i)) :: !res;
        status.(i) <- Defined in
  for i = 0 to num_bindings - 1 do
    match status.(i) with
      Undefined -> emit_binding i
    | Inprogress -> assert false
    | Defined -> ()
  done;
  List.rev !res

(* Generate lambda-code for a reordered list of bindings *)

let eval_rec_bindings phase bindings cont =
  let rec bind_inits = function
    [] ->
      bind_strict bindings
  | (_id, None, _rhs) :: rem ->
      bind_inits rem
  | (id, Some(loc, shape), _rhs) :: rem ->
      Llet(Strict, Pgenval, id,
           Lapply{ap_should_be_tailcall=false;
                  ap_loc=Location.none;
                  ap_func=mod_prim phase "init_mod";
                  ap_args=[loc; shape];
                  ap_inlined=Default_inline;
                  ap_specialised=Default_specialise},
           bind_inits rem)
  and bind_strict = function
    [] ->
      patch_forwards bindings
  | (id, None, rhs) :: rem ->
      Llet(Strict, Pgenval, id, rhs, bind_strict rem)
  | (_id, Some _, _rhs) :: rem ->
      bind_strict rem
  and patch_forwards = function
    [] ->
      cont
  | (_id, None, _rhs) :: rem ->
      patch_forwards rem
  | (id, Some(_loc, shape), rhs) :: rem ->
      Lsequence(Lapply{ap_should_be_tailcall=false;
                       ap_loc=Location.none;
                       ap_func=mod_prim phase "update_mod";
                       ap_args=[shape; Lvar id; rhs];
                       ap_inlined=Default_inline;
                       ap_specialised=Default_specialise},
                patch_forwards rem)
  in
    bind_inits bindings

let compile_recmodule phase compile_rhs bindings cont =
  eval_rec_bindings phase
    (reorder_rec_bindings
       (List.map
          (fun {mb_id=id; mb_expr=modl; _} ->
            (id, modl.mod_loc, init_shape phase modl, compile_rhs id modl))
          bindings))
    cont

(* Extract the list of "value" identifiers bound by a signature.
   "Value" identifiers are identifiers for signature components that
   correspond to a run-time value: values, extensions, modules, classes.
   Note: manifest primitives do not correspond to a run-time value! *)


let rec bound_value_identifiers phase env = function
    [] -> []
  | Sig_value(id, sf, {val_kind = Val_reg}) :: rem ->
      if phase = sf then
        id :: bound_value_identifiers phase env rem
      else
        bound_value_identifiers phase env rem
  | Sig_value(id, _, {val_kind = Val_macro}) :: rem ->
      id :: bound_value_identifiers phase env rem
  | Sig_typext(id, _, _) :: rem -> id :: bound_value_identifiers phase env rem
  | Sig_module(id, _, sf, _) :: rem ->
      if phase = Nonstatic && sf = Static then
        bound_value_identifiers phase env rem
      else
        id :: bound_value_identifiers phase env rem
  | Sig_class(id, _, _) :: rem ->
      if phase = Static then
        bound_value_identifiers phase env rem
      else
        id :: bound_value_identifiers phase env rem
  | _ :: rem -> bound_value_identifiers phase env rem

(* Code to translate class entries in a structure *)

let transl_class_bindings cl_list =
  let ids = List.map (fun (ci, _) -> ci.ci_id_class) cl_list in
  (ids,
   List.map
     (fun ({ci_id_class=id; ci_expr=cl; ci_virt=vf}, meths) ->
       (id, transl_class ids id meths cl vf))
     cl_list)

(* Compile a module expression *)

let rec transl_module cc rootpath target_phase mexp =
  let loc = mexp.mod_loc in
  if Env.contains_phase_mty target_phase mexp.mod_env mexp.mod_type then begin
    List.iter (Translattribute.check_attribute_on_module mexp)
      mexp.mod_attributes;
    match mexp.mod_type with
      Mty_alias _ -> apply_coercion loc target_phase Alias cc lambda_unit
    | _ ->
        match mexp.mod_desc with
          Tmod_ident (path,_) ->
            apply_coercion loc target_phase Strict cc
              (transl_path ~loc
                (Env.with_phase (Env.phase_of_sf target_phase) mexp.mod_env)
                path)
        | Tmod_structure str ->
            fst (transl_struct loc [] cc rootpath target_phase str)
        | Tmod_functor(param, _, _, body) ->
            let bodypath = functor_path rootpath param in
            let inline_attribute =
              Translattribute.get_inline_attribute mexp.mod_attributes
            in
            oo_wrap mexp.mod_env true
              (function
                | Tcoerce_none ->
                    Lfunction{kind = Curried; params = [param];
                              attr = { inline = inline_attribute;
                                       specialise = Default_specialise;
                                       is_a_functor = true };
                              loc = loc;
                              body = transl_module Tcoerce_none bodypath
                                target_phase body}
                | Tcoerce_functor(ccarg, ccres) ->
                    let param' = Ident.create "funarg" in
                    Lfunction{kind = Curried; params = [param'];
                              attr = { inline = inline_attribute;
                                       specialise = Default_specialise;
                                       is_a_functor = true };
                              loc = loc;
                              body = Llet(Alias, Pgenval, param,
                                          apply_coercion loc target_phase Alias
                                            ccarg (Lvar param'),
                                          transl_module ccres bodypath
                                            target_phase body)}
                | _ ->
                    fatal_error "Translmod.transl_module")
              cc
        | Tmod_apply(funct, arg, ccarg) ->
            let inlined_attribute, funct =
              Translattribute.get_and_remove_inlined_attribute_on_module funct
            in
            oo_wrap mexp.mod_env true
              (apply_coercion loc target_phase Strict cc)
              (Lapply{ap_should_be_tailcall=false;
                      ap_loc=loc;
                      ap_func=transl_module Tcoerce_none None
                        target_phase funct;
                      ap_args=[transl_module ccarg None target_phase arg];
                      ap_inlined=inlined_attribute;
                      ap_specialised=Default_specialise})
        | Tmod_constraint(arg, _, _, ccarg) ->
            transl_module (compose_coercions target_phase cc ccarg)
              rootpath target_phase arg
        | Tmod_unpack(arg, _) ->
            if target_phase <> Static then
              apply_coercion loc target_phase Strict cc (Translcore.transl_exp arg)
            else zero_lam
  end else
    Lprim(Pmakeblock (0,Immutable,None), [], loc)

and transl_struct loc fields cc rootpath static_flag str =
  transl_structure loc fields cc rootpath static_flag
  (fun _ lam -> lam) str.str_final_env str.str_items

and transl_structure loc fields cc rootpath target_phase item_postproc final_env = function
    [] ->
      let body, size =
        match cc with
          Tcoerce_none ->
            Lprim(Pmakeblock(0, Immutable, None),
                  List.map (fun id -> Lvar id) (List.rev fields),
                  loc),
              List.length fields
        | Tcoerce_structure (pos_cc_list, id_pos_list) ->
                (* Do not ignore id_pos_list ! *)
            (*Format.eprintf "%a@.@[" Includemod.print_coercion cc;
            List.iter (fun l -> Format.eprintf "%a@ " Ident.print l)
              fields;
            Format.eprintf "@]@.";*)
            let fields = List.rev fields in
            let (pos_cc_list, id_pos_list) =
              (filter_pos_cc target_phase pos_cc_list,
                filter_id_pos target_phase id_pos_list)
            in
            let v = Array.of_list fields in
            let get_field pos =
              let id = v.(pos) in
              Lvar id
            and ids = List.fold_right IdentSet.add fields IdentSet.empty in
            let lam =
              Lprim(Pmakeblock(0, Immutable, None),
                  List.map
                    (fun (pos, cc) ->
                      match cc with
                        Tcoerce_primitive p ->
                          if target_phase = Nonstatic then
                            transl_primitive p.pc_loc
                              p.pc_desc p.pc_env p.pc_type None
                          else zero_lam
                      | _ -> apply_coercion loc target_phase Strict cc
                        (get_field pos))
                    pos_cc_list,
                  loc)
            and id_pos_list =
              List.filter (fun (id,_,_) -> not (IdentSet.mem id ids))
                id_pos_list
            in
            wrap_id_pos_list loc target_phase id_pos_list get_field lam,
              List.length pos_cc_list
        | _ ->
            fatal_error "Translmod.transl_structure"
      in
      (* This debugging event provides information regarding the structure
         items. It is ignored by the OCaml debugger but is used by
         Js_of_ocaml to preserve variable names. *)
      (if !Clflags.debug && not !Clflags.native_code then
         Levent(body,
                {lev_loc = loc;
                 lev_kind = Lev_pseudo;
                 lev_repr = None;
                 lev_env = Env.summary final_env})
       else
         body),
      size
  | item :: rem ->
      (* Determine whether an item should be translated or ignored, based on
         its static modifier, and the target phase. *)
      let should_translate sf item =
        let item_phase = Env.cur_phase item.str_env + Env.phase_of_sf sf in
        let target_phase = if target_phase = Static then 1 else 0 in
        item_phase = target_phase
      in
      let (item_lam, size) =
      match item.str_desc with
      | Tstr_eval (expr, _) ->
          (* toplevel expressions are deemed run-time *)
          if target_phase = Nonstatic then
            let body, size = transl_structure loc fields cc rootpath
              target_phase item_postproc final_env rem
            in
            Lsequence(transl_exp expr, body), size
          else
            transl_structure loc fields cc rootpath target_phase item_postproc
              final_env rem
      | Tstr_value(sf, rec_flag, pat_expr_list) -> begin
          if should_translate sf item then begin
            let ext_fields = rev_let_bound_idents pat_expr_list @ fields in
            let body, size =
              transl_structure loc ext_fields cc rootpath target_phase
                item_postproc final_env rem in
            transl_let rec_flag pat_expr_list body, size
          end else
            let ext_fields = fields in
            transl_structure loc ext_fields cc rootpath target_phase
              item_postproc final_env rem
      end
      | Tstr_macro(rec_flag, pat_expr_list) ->
          let ext_fields = rev_let_bound_idents pat_expr_list @ fields in
          let body, size =
            transl_structure loc ext_fields cc rootpath target_phase
              item_postproc final_env rem
          in
          transl_macro target_phase rec_flag pat_expr_list
            TreeInspect.expression body,
          size
      | Tstr_primitive descr ->
          (if target_phase = Nonstatic then
            record_primitive descr.val_val
          );
          transl_structure loc fields cc rootpath target_phase
            item_postproc final_env rem
      | Tstr_type _ ->
          transl_structure loc fields cc rootpath target_phase
            item_postproc final_env rem
      | Tstr_typext(tyext) ->
          let ids = List.map (fun ext -> ext.ext_id) tyext.tyext_constructors in
          let body, size =
            transl_structure loc (List.rev_append ids fields)
              cc rootpath target_phase item_postproc final_env rem
          in
          transl_type_extension item.str_env rootpath tyext body, size
      | Tstr_exception ext ->
          let id = ext.ext_id in
          let path = field_path rootpath id in
          let body, size =
            transl_structure loc (id :: fields) cc rootpath target_phase
              item_postproc final_env rem
          in
          Llet(Strict, Pgenval, id,
               transl_extension_constructor item.str_env path ext,
               body), size
      | Tstr_module (sf, mb) ->
          if sf = Static && target_phase = Nonstatic then
            transl_structure loc fields cc rootpath target_phase item_postproc
              final_env rem
          else
            let id = mb.mb_id in
            let body, size =
              transl_structure loc (id :: fields) cc rootpath target_phase
                item_postproc final_env rem
            in
            let mod_expr =
              if sf = Static then
                {mb.mb_expr with mod_env = Env.with_phase 1 mb.mb_expr.mod_env}
              else mb.mb_expr
            in
            let module_body =
              (* avoid checking inline attributes twice *)
              (if target_phase = Nonstatic || sf = Static then
                Translattribute.add_inline_attribute
              else
                fun lam _ _ -> lam)
                  (transl_module Tcoerce_none (field_path rootpath id)
                    target_phase mod_expr)
                  mb.mb_loc mb.mb_attributes
            in
            Llet(pure_module mb.mb_expr, Pgenval, id,
                 module_body,
                 body), size
      | Tstr_recmodule (sf, bindings) ->
          if sf = Static && target_phase = Nonstatic then
            transl_structure loc fields cc rootpath target_phase item_postproc
              final_env rem
          else
            let ext_fields =
              List.rev_append (List.map (fun mb -> mb.mb_id) bindings) fields
            in
            let body, size =
              transl_structure loc ext_fields cc rootpath target_phase
                item_postproc final_env rem
            in
            let lam =
              compile_recmodule target_phase
                (fun id modl ->
                   let modl =
                     if sf = Static then
                       {modl with mod_env = Env.with_phase 1 modl.mod_env}
                     else modl
                   in
                   transl_module Tcoerce_none (field_path rootpath id) target_phase modl)
                bindings
                body
            in
            lam, size
      | Tstr_class cl_list ->
          let (ids, class_bindings) = transl_class_bindings cl_list in
          if target_phase = Static then
            transl_structure loc fields cc rootpath target_phase item_postproc
              final_env rem
          else
            let body, size =
              transl_structure loc (List.rev_append ids fields)
                cc rootpath target_phase
                  item_postproc final_env rem
            in
            Lletrec(class_bindings, body), size
      | Tstr_include incl ->
          let ids = bound_value_identifiers target_phase item.str_env
            incl.incl_type
          in
          let modl = incl.incl_mod in
          let mid = Ident.create "include" in
          let rec rebind_idents pos newfields = function
              [] ->
                transl_structure loc newfields cc rootpath target_phase
                  item_postproc final_env rem
            | id :: ids ->
                let body, size =
                  rebind_idents (pos + 1) (id :: newfields) ids
                in
                Llet(Alias, Pgenval, id,
                     Lprim(Pfield pos, [Lvar mid], incl.incl_loc), body),
                size
          in
          let body, size = rebind_idents 0 fields ids in
          Llet(pure_module modl, Pgenval, mid,
               transl_module Tcoerce_none None target_phase modl, body),
          size
      | Tstr_modtype _
      | Tstr_open _
      | Tstr_class_type _
      | Tstr_attribute _ ->
          transl_structure loc fields cc rootpath target_phase
            item_postproc final_env rem
      in
      (item_postproc item item_lam, size)

and pure_module m =
  match m.mod_desc with
    Tmod_ident _ -> Alias
  | Tmod_constraint (m,_,_,_) -> pure_module m
  | _ -> Strict

(* Update forward declaration in Translcore *)
let _ =
  Translcore.transl_module := fun cc rp m -> transl_module cc rp Nonstatic m

(* Introduce dependencies on modules referenced only by "external". *)

let scan_used_globals lam =
  let globals = ref Ident.Set.empty in
  let rec scan lam =
    Lambda.iter scan lam;
    match lam with
      Lprim ((Pgetglobal id | Psetglobal id), _, _) ->
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
    Hashtbl.fold
      (fun path _ -> add_global (Path.head path)) used_primitives
      (if flambda then globals else Ident.Set.empty)
  in
  let required =
    List.fold_right add_global (Env.get_required_globals ()) required
  in
  Env.reset_required_globals ();
  Hashtbl.clear used_primitives;
  required

(* Compile an implementation *)

let transl_implementation_flambda module_name (str, cc) =
  reset_labels ();
  primitive_declarations := [];
  Hashtbl.clear used_primitives;
  let module_id = Ident.create_persistent module_name in
  let body, size =
    Translobj.transl_label_init
      (fun () -> transl_struct Location.none [] cc (global_path module_id)
                   Nonstatic str)
  in
  { module_ident = module_id;
    main_module_block_size = size;
    required_globals = required_globals ~flambda:true body;
    code = body }

let transl_implementation module_name target_phase (str, cc) =
  if target_phase = Static then
    let module_id = Ident.create_persistent module_name in
    Translcore.set_transl_splices None;
    splice_ids := [];
    let (mod_body, size) =
      transl_structure Location.none [] cc (Some (Path.Pident module_id))
        Static transl_item_splices str.str_final_env str.str_items
    in
    let code =
      insert_splice_array module_id !splice_ids mod_body
    in
    { module_ident = module_id;
      main_module_block_size = size;
      required_globals = required_globals ~flambda:false code;
      code = code }
  else
    let implementation =
      transl_implementation_flambda module_name (str, cc)
    in
    let code =
      Lprim (Psetglobal implementation.module_ident, [implementation.code],
             Location.none)
    in
    { implementation with code }

(* Build the list of value identifiers defined by a toplevel structure
   (excluding primitive declarations and replacing different-phase identifiers
    with placeholders). *)

let rec defined_idents static_flag = function
    [] -> []
  | item :: rem ->
    match item.str_desc with
    | Tstr_eval _ -> defined_idents static_flag rem
    | Tstr_value(sf, _rec_flag, pat_expr_list) -> (
      if sf = static_flag then
        let_bound_idents pat_expr_list @ defined_idents static_flag rem
      else
        defined_idents static_flag rem
    )
    | Tstr_macro (_rec_flag, pat_expr_list) ->
        let_bound_idents pat_expr_list @ defined_idents static_flag rem
    | Tstr_primitive _ -> defined_idents static_flag rem
    | Tstr_type _ -> defined_idents static_flag rem
    | Tstr_typext tyext ->
      List.map (fun ext -> ext.ext_id) tyext.tyext_constructors
      @ defined_idents static_flag rem
    | Tstr_exception ext -> ext.ext_id :: defined_idents static_flag rem
    | Tstr_module (sf, mb) ->
        begin match (static_flag, sf) with
        | (Nonstatic, Static) ->
          defined_idents static_flag rem
        | _ (* target phase is higher than module phase *) ->
          mb.mb_id :: defined_idents static_flag rem
        end
    | Tstr_recmodule (sf, decls) ->
        begin match (static_flag, sf) with
        | (Nonstatic, Static) ->
          defined_idents static_flag rem
        | _ (* target phase is higher than module phase *) ->
          List.map (fun mb -> mb.mb_id) decls @ defined_idents static_flag rem
        end
    | Tstr_modtype _ -> defined_idents static_flag rem
    | Tstr_open _ -> defined_idents static_flag rem
    | Tstr_class cl_list ->
      List.map (fun (ci, _) -> ci.ci_id_class) cl_list @ defined_idents static_flag rem
    | Tstr_class_type _ -> defined_idents static_flag rem
    | Tstr_include incl ->
      bound_value_identifiers static_flag item.str_env incl.incl_type @
        defined_idents static_flag rem
    | Tstr_attribute _ -> defined_idents static_flag rem

(* second level idents (module M = struct ... let id = ... end),
   and all sub-levels idents *)
let rec more_idents target_phase = function
    [] -> []
  | item :: rem ->
    match item.str_desc with
    | Tstr_eval _ -> more_idents target_phase rem
    | Tstr_value _ -> more_idents target_phase rem
    | Tstr_macro _ -> more_idents target_phase rem
    | Tstr_primitive _ -> more_idents target_phase rem
    | Tstr_type _ -> more_idents target_phase rem
    | Tstr_typext _ -> more_idents target_phase rem
    | Tstr_exception _ -> more_idents target_phase rem
    | Tstr_recmodule _ -> more_idents target_phase rem
    | Tstr_modtype _ -> more_idents target_phase rem
    | Tstr_open _ -> more_idents target_phase rem
    | Tstr_class _ -> more_idents target_phase rem
    | Tstr_class_type _ -> more_idents target_phase rem
    | Tstr_include _ -> more_idents target_phase rem
    | Tstr_module (sf, {mb_expr={mod_desc = Tmod_structure str}})
    | Tstr_module (sf, {mb_expr={mod_desc =
                             Tmod_constraint ({mod_desc = Tmod_structure str},
                                              _, _, _)}}) ->
      begin match (target_phase, sf) with
      | (Nonstatic, Static) ->
        more_idents target_phase rem
      | _ ->
        all_idents target_phase str.str_items @ more_idents target_phase rem
      end
    | Tstr_module _ -> more_idents target_phase rem
    | Tstr_attribute _ -> more_idents target_phase rem

and all_idents target_phase = function
    [] -> []
  | item :: rem ->
    match item.str_desc with
    | Tstr_eval _ -> all_idents target_phase rem
    | Tstr_value(sf, _rec_flag, pat_expr_list) ->
      begin match (target_phase, sf) with
        | (Nonstatic, Static) -> all_idents target_phase rem
        | _ -> let_bound_idents pat_expr_list @ all_idents target_phase rem
      end
    | Tstr_macro (_rec_flag, pat_expr_list) ->
        if target_phase = Static then
          let_bound_idents pat_expr_list @ all_idents target_phase rem
        else all_idents target_phase rem
    | Tstr_primitive _ -> all_idents target_phase rem
    | Tstr_type _ -> all_idents target_phase rem
    | Tstr_typext tyext ->
      List.map (fun ext -> ext.ext_id) tyext.tyext_constructors
      @ all_idents target_phase rem
    | Tstr_exception ext -> ext.ext_id :: all_idents target_phase rem
    | Tstr_recmodule (sf, decls) ->
      begin match (target_phase, sf) with
      | (Nonstatic, Static) -> all_idents target_phase rem
      | _ ->
        List.map (fun mb -> mb.mb_id) decls @ all_idents target_phase rem
      end
    | Tstr_modtype _ -> all_idents target_phase rem
    | Tstr_open _ -> all_idents target_phase rem
    | Tstr_class cl_list ->
      List.map (fun (ci, _) -> ci.ci_id_class) cl_list @ all_idents target_phase rem
    | Tstr_class_type _ -> all_idents target_phase rem
    | Tstr_include incl ->
      bound_value_identifiers target_phase item.str_env incl.incl_type @
        all_idents target_phase rem
    | Tstr_module (sf, {mb_id;mb_expr={mod_desc = Tmod_structure str}})
    | Tstr_module (sf, {mb_id;
                        mb_expr={mod_desc =
                             Tmod_constraint ({mod_desc = Tmod_structure str},
                                              _, _, _)}}) ->
      begin match (target_phase, sf) with
      | (Nonstatic, Static) -> all_idents target_phase rem
      | _ ->
        mb_id :: all_idents target_phase str.str_items @ all_idents target_phase rem
      end
    | Tstr_module (sf,mb) ->
      begin match (target_phase, sf) with
      | (Nonstatic, Static) -> all_idents target_phase rem
      | _ -> mb.mb_id :: all_idents target_phase rem
      end
    | Tstr_attribute _ -> all_idents target_phase rem


(* A variant of transl_structure used to compile toplevel structure definitions
   for the native-code compiler. Store the defined values in the fields
   of the global as soon as they are defined, in order to reduce register
   pressure.  Also rewrites the defining expressions so that they
   refer to earlier fields of the structure through the fields of
   the global, not by their names.
   "map" is a table from defined idents to (pos in global block, coercion).
   "prim" is a list of (pos in global block, primitive declaration). *)

let transl_store_subst = ref Ident.empty
  (** In the native toplevel, this reference is threaded through successive
      calls of transl_store_structure *)

let nat_toplevel_name id =
  try match Ident.find_same id !transl_store_subst with
    | Lprim(Pfield pos, [Lprim(Pgetglobal glob, [], _)], _) -> (glob,pos)
    | _ -> raise Not_found
  with Not_found ->
    fatal_error("Translmod.nat_toplevel_name: " ^ Ident.unique_name id)

let transl_store_structure target_phase glob map prims str =
  let rec transl_store rootpath subst = function
    [] ->
      transl_store_subst := subst;
        lambda_unit
    | item :: rem ->
        let should_translate sf item =
          let item_phase = Env.cur_phase item.str_env + Env.phase_of_sf sf in
          let target_phase = if target_phase = Static then 1 else 0 in
          item_phase = target_phase
        in
        match item.str_desc with
        | Tstr_eval (expr, _attrs) ->
            (* top-level expression are deemed run-time *)
            if target_phase = Nonstatic then
              Lsequence(subst_lambda subst (transl_exp expr),
                        transl_store rootpath subst rem)
            else
              transl_store rootpath subst rem
        | Tstr_value(sf, rec_flag, pat_expr_list) ->
            if should_translate sf item then
              let ids = let_bound_idents pat_expr_list in
              let lam =
                transl_let rec_flag pat_expr_list
                  (store_idents Location.none ids)
              in
              Lsequence(subst_lambda subst lam,
                        transl_store rootpath (add_idents false ids subst) rem)
            else
              transl_store rootpath subst rem
        | Tstr_macro (rec_flag, pat_expr_list) ->
            let ids = let_bound_idents pat_expr_list in
            let lam =
              transl_macro target_phase rec_flag pat_expr_list
                TreeInspect.expression (store_idents Location.none ids)
            in
            Lsequence (subst_lambda subst lam,
              transl_store rootpath (add_idents false ids subst) rem)
        | Tstr_primitive descr ->
            begin if target_phase = Nonstatic then
              record_primitive descr.val_val
            end;
            transl_store rootpath subst rem
        | Tstr_type _ ->
            transl_store rootpath subst rem
        | Tstr_typext(tyext) ->
            let ids =
              List.map (fun ext -> ext.ext_id) tyext.tyext_constructors
            in
            let lam =
              transl_type_extension item.str_env rootpath tyext
                                    (store_idents Location.none ids)
            in
            Lsequence(subst_lambda subst lam,
                      transl_store rootpath (add_idents false ids subst) rem)
        | Tstr_exception ext ->
            let id = ext.ext_id in
            let path = field_path rootpath id in
            let lam = transl_extension_constructor item.str_env path ext in
            Lsequence(Llet(Strict, Pgenval, id, subst_lambda subst lam,
                           store_ident ext.ext_loc id),
                      transl_store rootpath (add_ident false id subst) rem)
        | Tstr_module (sf, {mb_id=id;mb_loc=loc;
                            mb_expr={mod_desc = Tmod_structure str} as mexp;
                            mb_attributes}) ->
            List.iter (Translattribute.check_attribute_on_module mexp)
              mb_attributes;
            if sf = Static && target_phase = Nonstatic then
              transl_store rootpath subst rem
            else begin
              let lam =
                transl_store (field_path rootpath id) subst str.str_items
              in
              (* Careful: see next case *)
              let subst = !transl_store_subst in
              Lsequence(lam,
                        Llet(Strict, Pgenval, id,
                             subst_lambda subst
                               (Lprim(Pmakeblock(0, Immutable, None),
                                      List.map (fun id -> Lvar id)
                                        (defined_idents target_phase
                                          str.str_items), loc)),
                             Lsequence(store_ident loc id,
                                       transl_store rootpath
                                                    (add_ident true id subst)
                                                    rem)))
            end
        | Tstr_module (sf, {
            mb_id=id; mb_loc=loc;
            mb_expr= {
              mod_desc = Tmod_constraint (
                  {mod_desc = Tmod_structure str} as mexp, _, _,
                  (Tcoerce_structure (map, _) as _cc))};
            mb_attributes
          }) ->
            List.iter (Translattribute.check_attribute_on_module mexp)
              mb_attributes;
            if sf = Static && target_phase = Nonstatic then
              transl_store rootpath subst rem
            else begin
              (*    Format.printf "coerc id %s: %a@." (Ident.unique_name id)
                                  Includemod.print_coercion cc; *)
              let lam =
                transl_store (field_path rootpath id) subst str.str_items
              in
              (* Careful: see next case *)
              let subst = !transl_store_subst in
              let ids =
                Array.of_list (defined_idents target_phase str.str_items)
              in
              let field (pos, cc) =
                match cc with
                | Tcoerce_primitive { pc_loc; pc_desc; pc_env; pc_type; } ->
                    transl_primitive pc_loc pc_desc pc_env pc_type None
                | _ -> apply_coercion loc target_phase Strict cc (Lvar ids.(pos))
              in
              let map = filter_pos_cc target_phase map in
              Lsequence(lam,
                        Llet(Strict, Pgenval, id,
                             subst_lambda subst
                               (Lprim(Pmakeblock(0, Immutable, None),
                                      List.map field map, loc)),
                             Lsequence(store_ident loc id,
                                       transl_store rootpath
                                                    (add_ident true id subst)
                                                    rem)))
            end
        | Tstr_module (sf, {mb_id=id;mb_expr=modl;mb_loc=loc;mb_attributes}) ->
            if sf = Static && target_phase = Nonstatic then
              transl_store rootpath subst rem
            else
              let modl =
                if sf = Static then
                  {modl with mod_env = Env.with_phase 1 modl.mod_env}
                else modl
              in
              let lam =
                (* avoid checking inline attributes twice *)
                (if target_phase = Nonstatic || sf = Static then
                  Translattribute.add_inline_attribute
                else
                  fun lam _ _ -> lam)
                    (transl_module Tcoerce_none (field_path rootpath id)
                      target_phase modl)
                    loc mb_attributes
              in
              (* Careful: the module value stored in the global may be different
                 from the local module value, in case a coercion is applied.
                 If so, keep using the local module value (id) in the remainder of
                 the compilation unit (add_ident true returns subst unchanged).
                 If not, we can use the value from the global
                 (add_ident true adds id -> Pgetglobal... to subst). *)
              Llet(Strict, Pgenval, id, subst_lambda subst lam,
                   Lsequence(store_ident loc id,
                             transl_store rootpath (add_ident true id subst) rem))
        | Tstr_recmodule (sf, bindings) ->
            if sf = Static && target_phase = Nonstatic then
              transl_store rootpath subst rem
            else
              let ids = List.map (fun mb -> mb.mb_id) bindings in
              compile_recmodule target_phase
                (fun id modl ->
                   let modl =
                     if sf = Static then
                       {modl with mod_env = Env.with_phase 1 modl.mod_env}
                     else modl
                   in
                   subst_lambda subst
                     (transl_module Tcoerce_none
                        (field_path rootpath id) target_phase modl))
                bindings
                (Lsequence(store_idents Location.none ids,
                           transl_store rootpath (add_idents true ids subst) rem))
        | Tstr_class cl_list ->
            let (ids, class_bindings) = transl_class_bindings cl_list in
            if target_phase = Static then
              transl_store rootpath subst rem
            else
              let lam =
                Lletrec(class_bindings, store_idents Location.none ids)
              in
              Lsequence(subst_lambda subst lam,
                        transl_store rootpath (add_idents false ids subst) rem)
        | Tstr_include incl ->
            let ids = bound_value_identifiers target_phase item.str_env
              incl.incl_type
            in
            let modl = incl.incl_mod in
            let mid = Ident.create "include" in
            let loc = incl.incl_loc in
            let rec store_idents pos = function
                [] -> transl_store rootpath (add_idents true ids subst) rem
              | id :: idl ->
                  Llet(Alias, Pgenval, id, Lprim(Pfield pos, [Lvar mid], loc),
                       Lsequence(store_ident loc id,
                                 store_idents (pos + 1) idl))
            in
            Llet(Strict, Pgenval, mid,
                 subst_lambda subst (transl_module Tcoerce_none None Nonstatic modl),
                 store_idents 0 ids)
        | Tstr_modtype _
        | Tstr_open _
        | Tstr_class_type _
        | Tstr_attribute _ ->
            transl_store rootpath subst rem

  and store_ident loc id =
    try
      let (pos, cc) = Ident.find_same id map in
      let init_val = apply_coercion loc Nonstatic Alias cc (Lvar id) in
      Lprim(Psetfield(pos, Pointer, Initialization),
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
          Ident.add id
            (Lprim(Pfield pos,
                   [Lprim(Pgetglobal glob, [], Location.none)],
                   Location.none))
            subst
      | _ ->
          if may_coerce then subst else assert false
    with Not_found ->
      assert false

  and add_idents may_coerce idlist subst =
    List.fold_right (add_ident may_coerce) idlist subst

  and store_primitive (pos, prim) cont =
    Lsequence(Lprim(Psetfield(pos, Pointer, Initialization),
                    [Lprim(Pgetglobal glob, [], Location.none);
                     transl_primitive Location.none
                       prim.pc_desc prim.pc_env prim.pc_type None],
                    Location.none),
              cont)

  in List.fold_right store_primitive prims
                     (transl_store (global_path glob) !transl_store_subst str)

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
  let rec natural_map pos map prims = function
      [] ->
        (map, prims, pos)
    | id :: rem ->
        natural_map (pos+1) (Ident.add id (pos, Tcoerce_none) map) prims rem in
  let (map, prims, pos) =
    match restr with
        Tcoerce_none ->
          natural_map 0 Ident.empty [] idlist
      | Tcoerce_structure (pos_cc_list, _id_pos_list) ->
        (* consider phase is Nonstatic since this function is used by
         * ocamlopt *)
        let pos_cc_list = filter_pos_cc Nonstatic pos_cc_list in
              (* ignore _id_pos_list as the ids are already bound *)
        let idarray = Array.of_list idlist in
        let rec export_map pos map prims undef = function
          [] ->
          natural_map pos map prims undef
        | (_source_pos, Tcoerce_primitive p) :: rem ->
          export_map (pos + 1) map ((pos, p) :: prims) undef rem
        | (source_pos, cc) :: rem ->
          let id = idarray.(source_pos) in
          export_map (pos + 1) (Ident.add id (pos, cc) map)
            prims (list_remove id undef) rem
        in export_map 0 Ident.empty [] idlist pos_cc_list
      | _ ->
        fatal_error "Translmod.build_ident_map"
  in
  natural_map pos map prims more_ids

(* Compile an implementation using transl_store_structure
   (for the native-code compiler). *)

let transl_store_gen target_phase module_name ({ str_items = str }, restr) topl =
  reset_labels ();
  primitive_declarations := [];
  Hashtbl.clear used_primitives;
  let module_id = Ident.create_persistent module_name in
  let (map, prims, size) =
    build_ident_map restr (defined_idents target_phase str)
      (more_idents target_phase str) in
  let f = function
    | [ { str_desc = Tstr_eval (expr, _attrs) } ] when topl ->
        assert (size = 0);
        subst_lambda !transl_store_subst (transl_exp expr)
    | str -> transl_store_structure target_phase module_id map prims str in
  transl_store_label_init module_id size f str
  (*size, transl_label_init (transl_store_structure module_id map prims str)*)

let transl_store_phrases target_phase module_name str =
  transl_store_gen target_phase module_name (str,Tcoerce_none) true

let transl_store_implementation target_phase module_name (str, restr) =
  let s = !transl_store_subst in
  transl_store_subst := Ident.empty;
  let (i, code) =
    transl_store_gen target_phase module_name (str, restr) false
  in
  transl_store_subst := s;
  { Lambda.main_module_block_size = i;
    code;
    (* module_ident is not used by closure, but this allow to share
       the type with the flambda version *)
    module_ident = Ident.create_persistent module_name;
    required_globals = required_globals ~flambda:true code }

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

let toploop_getvalue phase id =
  let phase =
    match phase with Static -> 1 | Nonstatic -> 0
  in
  Lapply{ap_should_be_tailcall=false;
         ap_loc=Location.none;
         ap_func=Lprim(Pfield toploop_getvalue_pos,
                       [Lprim(Pgetglobal toploop_ident, [], Location.none)],
                       Location.none);
         ap_args=[
           Lconst(Const_base(Const_int phase));
           Lconst(Const_base(Const_string (toplevel_name id, None)))];
         ap_inlined=Default_inline;
         ap_specialised=Default_specialise}

let toploop_setvalue phase id lam =
  let phase =
    match phase with Static -> 1 | Nonstatic -> 0
  in
  Lapply{ap_should_be_tailcall=false;
         ap_loc=Location.none;
         ap_func=Lprim(Pfield toploop_setvalue_pos,
                       [Lprim(Pgetglobal toploop_ident, [], Location.none)],
                       Location.none);
         ap_args=[
           Lconst(Const_base(Const_int phase));
           Lconst(Const_base(Const_string (toplevel_name id, None)));
                  lam];
         ap_inlined=Default_inline;
         ap_specialised=Default_specialise}

let toploop_setvalue_id phase id = toploop_setvalue phase id (Lvar id)

let close_toplevel_term target_phase (lam, ()) =
  IdentSet.fold (fun id l -> Llet(Strict, Pgenval, id,
                                  toploop_getvalue target_phase id, l))
                (free_variables lam) lam

let transl_toplevel_item target_phase item =
  let item_lam =
    match item.str_desc with
      Tstr_eval (expr, _) ->
        if target_phase = Nonstatic then
          transl_exp expr
        else lambda_unit
    | Tstr_value(sf, Nonrecursive,
                 [{vb_pat = {pat_desc=Tpat_any};vb_expr = expr}]) ->
        (* special compilation for toplevel "let _ = expr" or "static _ =
           expr", so that Toploop can display the result of the expression.
           Otherwise, the normal compilation would result in a Lsequence
           returning unit. *)
        if sf = target_phase then
          transl_exp expr
        else lambda_unit
    | Tstr_value(static_flag, rec_flag, pat_expr_list) ->
        if static_flag = target_phase then
          let idents = let_bound_idents pat_expr_list in
          transl_let rec_flag pat_expr_list
            (make_sequence (toploop_setvalue_id target_phase) idents)
        else lambda_unit
    | Tstr_macro (rec_flag, pat_expr_list) ->
        let idents = let_bound_idents pat_expr_list in
        transl_macro target_phase rec_flag pat_expr_list
          TreeInspect.expression
          (make_sequence (toploop_setvalue_id target_phase) idents)
    | Tstr_typext(tyext) ->
        if target_phase = Nonstatic then
          let idents =
            List.map (fun ext -> ext.ext_id) tyext.tyext_constructors
          in
          (* we need to use unique name in case of multiple
             definitions of the same extension constructor in the toplevel *)
          List.iter set_toplevel_unique_name idents;
            transl_type_extension item.str_env None tyext
              (make_sequence (toploop_setvalue_id target_phase) idents)
        else lambda_unit
    | Tstr_exception ext ->
        set_toplevel_unique_name ext.ext_id;
        toploop_setvalue target_phase ext.ext_id
          (transl_extension_constructor item.str_env None ext)
    | Tstr_module (sf, {mb_id=id; mb_expr=modl}) ->
        if sf = Static && target_phase = Nonstatic then
          lambda_unit
        else begin
          (* we need to use the unique name for the module because of issues
             with "open" (PR#1672) *)
          set_toplevel_unique_name id;
          let lam =
            transl_module Tcoerce_none (Some(Pident id)) target_phase modl
          in
          toploop_setvalue target_phase id lam
        end
    | Tstr_recmodule (sf, bindings) ->
        if sf = Static && target_phase = Nonstatic then
          lambda_unit
        else begin
          let idents = List.map (fun mb -> mb.mb_id) bindings in
          compile_recmodule target_phase
            (fun id modl ->
              transl_module Tcoerce_none (Some(Pident id)) target_phase modl)
            bindings
            (make_sequence (toploop_setvalue_id target_phase) idents)
        end
    | Tstr_class cl_list ->
        if target_phase = Nonstatic then begin
          (* we need to use unique names for the classes because there might
             be a value named identically *)
          let (ids, class_bindings) = transl_class_bindings cl_list in
          List.iter set_toplevel_unique_name ids;
          Lletrec(class_bindings,
            make_sequence (toploop_setvalue_id target_phase) ids)
        end
        else lambda_unit
    | Tstr_include incl ->
        let ids = bound_value_identifiers target_phase item.str_env 
          incl.incl_type in
        let modl = incl.incl_mod in
        let mid = Ident.create "include" in
        let rec set_idents pos = function
          [] ->
            lambda_unit
        | id :: ids ->
            Lsequence(
              toploop_setvalue target_phase id
                (Lprim(Pfield pos, [Lvar mid], Location.none)),
              set_idents (pos + 1) ids)
        in
        Llet(Strict, Pgenval, mid,
             transl_module Tcoerce_none None target_phase modl, set_idents 0 ids)
    | Tstr_modtype _
    | Tstr_open _
    | Tstr_primitive _
    | Tstr_type _
    | Tstr_class_type _
    | Tstr_attribute _ ->
        lambda_unit
  in
  if target_phase = Static then begin
    TranslSplices.iter_structure_item item;
    let item_splices_with_ids =
      List.map (fun l -> (Ident.create "*splice*", l)) !item_splices
    in
    let wrap_let str_lam (splice_id, splice_lam) =
      Llet (Strict, Pgenval, splice_id, splice_lam, str_lam)
    in
    let lam = List.fold_left wrap_let item_lam item_splices_with_ids in
    item_splices := [];
    splice_ids := !splice_ids @ List.map fst item_splices_with_ids;
    lam
  end
  else
    item_lam

let transl_toplevel_item_and_close target_phase itm =
  close_toplevel_term target_phase
    (transl_label_init (fun () -> transl_toplevel_item target_phase itm, ()))

let rec insert_splice_array_toplevel splice_ids = function
  | Llet (a,b,c,lam,rem) ->
      Llet (a,b,c,lam, insert_splice_array_toplevel splice_ids rem)
  | Lletrec (vbs, rem) ->
      Lletrec (vbs, insert_splice_array_toplevel splice_ids rem)
  | other ->
      let splice_body =
        Lprim (Pmakearray (Paddrarray, Mutable),
          List.map
            (fun id ->
              Translquote.transl_close_expression Location.none
              (Lvar id))
            splice_ids,
          Location.none)
      in
      Lsequence (
        other,
        splice_body)

let transl_toplevel_definition target_phase str =
  reset_labels ();
  Hashtbl.clear used_primitives;
  if target_phase = Static then begin
    Translcore.set_transl_splices None;
    splice_ids := [];
    let lam =
      make_sequence (transl_toplevel_item_and_close target_phase) str.str_items
    in
    insert_splice_array_toplevel !splice_ids lam
  end
  else
    make_sequence (transl_toplevel_item_and_close target_phase) str.str_items

(* Compile the initialization code for a packed library *)

let get_component = function
    None -> Lconst const_unit
  | Some id -> Lprim(Pgetglobal id, [], Location.none)

let transl_package_flambda component_names coercion =
  let size =
    match coercion with
    | Tcoerce_none -> List.length component_names
    | Tcoerce_structure (l, _) -> List.length l
    | Tcoerce_functor _
    | Tcoerce_primitive _
    | Tcoerce_alias _ -> assert false
  in
  size,
  apply_coercion Location.none Nonstatic Strict coercion
    (Lprim(Pmakeblock(0, Immutable, None),
           List.map get_component component_names,
           Location.none))

let transl_package component_names target_name coercion =
  let components =
    Lprim(Pmakeblock(0, Immutable, None),
          List.map get_component component_names, Location.none) in
  Lprim(Psetglobal target_name,
        [apply_coercion Location.none Nonstatic Strict coercion components],
        Location.none)
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
           Lprim(Psetfield(pos, Pointer, Initialization),
                 [Lprim(Pgetglobal target_name, [], Location.none);
                  get_component id],
                 Location.none))
         0 component_names)
  | Tcoerce_structure (pos_cc_list, _id_pos_list) ->
      let components =
        Lprim(Pmakeblock(0, Immutable, None),
              List.map get_component component_names,
              Location.none)
      in
      let blk = Ident.create "block" in
      (List.length pos_cc_list,
       Llet (Strict, Pgenval, blk,
             apply_coercion Location.none Nonstatic Strict coercion components,
             make_sequence
               (fun pos _id ->
                 Lprim(Psetfield(pos, Pointer, Initialization),
                       [Lprim(Pgetglobal target_name, [], Location.none);
                        Lprim(Pfield pos, [Lvar blk], Location.none)],
                       Location.none))
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

open Format

let report_error ppf = function
    Circular_dependency id ->
      fprintf ppf
        "@[Cannot safely evaluate the definition@ \
         of the recursively-defined module %a@]"
        Printtyp.ident id

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer loc report_error err)
      | _ ->
        None
    )

let reset () =
  primitive_declarations := [];
  transl_store_subst := Ident.empty;
  toploop_ident.Ident.flags <- 0;
  aliased_idents := Ident.empty;
  Env.reset_required_globals ();
  Hashtbl.clear used_primitives
