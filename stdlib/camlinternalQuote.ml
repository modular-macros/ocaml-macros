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

module Identifier =
struct
  open Effect
  open Effect.Deep
  module IdentSet = Set.Make(CamlinternalLam.Ident)
  type set_of_t = IdentSet.t

  type _ Effect.t += FreeVar: IdentSet.t -> unit Effect.t
  type _ Effect.t += Mute: IdentSet.t -> unit Effect.t

  type t = CamlinternalLam.Ident.t

  (* Renamed binders have negative stamps.  The compiler's stamps are
     positive, and code values carry the compiler's identifiers, so a
     renamed binder never coincides with one of them. *)
  let currentstamp = ref 0

  (* As [Ident.rename], counting down *)
  let rename = let open CamlinternalLam.Ident in function
  | Local { name; stamp = _ }
  | Scoped { name; stamp = _; scope = _ } ->
     decr currentstamp;
     Local { name; stamp = !currentstamp }
  | Global name | Predef { name ; _ } ->
     failwith ("CamlinternalQuote.rename " ^ name)
  | Unscoped us ->
     (* Unscoped idents name module-type-level parameters; they cannot be
        binders in quoted value code. *)
     let CamlinternalLam.Ident.Unscoped.{ name; stamp = _ } =
       CamlinternalLam.Ident.Unscoped.get_desc us
     in
     failwith ("CamlinternalQuote.rename " ^ name)

  let fresh name = rename (CamlinternalLam.Ident.Local { name; stamp = 0 })

  let check vars = if not (IdentSet.is_empty vars) then perform (FreeVar vars)

  let exec_in_current_scope scope f =
    let muted_vars = ref IdentSet.empty in
    match f () with
     effect FreeVar fvs, k->
       let free = IdentSet.diff fvs (IdentSet.union (!muted_vars) scope) in
       check free;
       continue k ()
   | effect Mute (fvs), k ->
       muted_vars := IdentSet.union (!muted_vars) (fvs);
       continue k ()
   | effect op, k  ->
       (* Forward a user effect out of the quotation.  [Mute] tells the
          enclosing scopes not to report this scope's binders while
          control is outside; with no enclosing scope there is nothing
          to tell.  If the user's handler discontinues the continuation,
          the exception is raised at the perform point, inside the
          quotation. *)
       (try perform (Mute (IdentSet.union scope (!muted_vars)))
        with Effect.Unhandled (Mute _) -> ());
       muted_vars := IdentSet.empty ;
       (match perform op with
        | v -> continue k v
        | exception e -> discontinue k e)
   | c, fvs ->
       let free = IdentSet.diff fvs scope in
       check free;
       (c, free)

  let new_scope alphas f =
    exec_in_current_scope (IdentSet.of_list alphas) f

  let empty = IdentSet.empty

  let free_var var = IdentSet.singleton var

  let merge_free_vars fvs1 fvs2 = IdentSet.union fvs1 fvs2

  let names fvs =
    let name = let open CamlinternalLam.Ident in function
      | Local { name; stamp = _ }
      | Scoped { name; stamp = _; scope = _ }
      | Predef { name; stamp = _ } -> name
      | Global name -> name
      | Unscoped us ->
          let CamlinternalLam.Ident.Unscoped.{ name; stamp = _ } =
            CamlinternalLam.Ident.Unscoped.get_desc us
          in
          name
    in
    List.sort_uniq compare (List.map name (IdentSet.elements fvs))

end
