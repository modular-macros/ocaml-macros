module Identifier =
struct
  open Effect
  open Effect.Deep
  module IdentSet = Set.Make(CamlinternalLam.Ident)
  type set_of_t = Set.Make(CamlinternalLam.Ident).t

  type _ Effect.t += FreeVar: IdentSet.t -> unit Effect.t
  type _ Effect.t += Mute: IdentSet.t -> unit Effect.t

  type t = CamlinternalLam.Ident.t

  (* Oleg points out an issue that we need to take care about:
     if the counter is reset between different runs of the compiler
     then we may end up inadvertently moving a free variable between
     two binders with the same name.  It seems likely that various
     other mechanisms (e.g, scope extrusion) will catch the problem,
     but we should check carefully. *)
  let currentstamp = ref 0

  (* From typing/ident.ml *)
  let rename = let open CamlinternalLam.Ident in function
  | Local { name; stamp = _ }
  | Scoped { name; stamp = _; scope = _ } ->
     incr currentstamp;
     Local { name; stamp = !currentstamp }
  | Global name | Predef { name ; _ } ->
     failwith ("CamlinternalQuote.rename " ^ name)

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
       perform (Mute (IdentSet.union scope (!muted_vars)));
       muted_vars := IdentSet.empty ;
       (match perform op with v -> continue k v)
   | c, fvs ->
       let free = IdentSet.diff fvs scope in
       check free;
       (c, free)

   let new_scope alphas f =
      let scope = IdentSet.of_list alphas in
      let c, fvs = exec_in_current_scope scope f in
      (c, fvs)

  let empty = IdentSet.empty

  let free_var var = IdentSet.singleton var

  let merge_free_vars fvs1 fvs2 = IdentSet.union fvs1 fvs2

end
