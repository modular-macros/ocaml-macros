module Identifier =
struct
  open Effect
  open Effect.Deep
  module IdentSet = Set.Make(CamlinternalLam.Ident)
  type set_of_t = IdentSet.t

  type _ Effect.t += FreeVar: IdentSet.t -> unit Effect.t
  type _ Effect.t += Mute: IdentSet.t -> unit Effect.t

  type t = CamlinternalLam.Ident.t

  let currentstamp = ref 0

  let rename = let open CamlinternalLam.Ident in function
  | Local { name; stamp = _ }
  | Scoped { name; stamp = _; scope = _ } ->
     decr currentstamp;
     Local { name; stamp = !currentstamp }
  | Global name | Predef { name ; _ } ->
     failwith ("CamlinternalQuote.rename " ^ name)
  | Unscoped us ->
     let CamlinternalLam.Ident.Unscoped.{ name; stamp = _ } =
       CamlinternalLam.Ident.Unscoped.get_desc us
     in
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
