module Identifier =
struct
  open Effect 
  open Effect.Deep 
  module IdentSet = Set.Make(CamlinternalLam.Ident)
  type set_of_t = Set.Make(CamlinternalLam.Ident).t

  type _ Effect.t += FreeVar: IdentSet.t -> unit Effect.t

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

  let exec_in_current_scope scope f = match f () with 
      effect FreeVar fvs, k-> let free = IdentSet.diff fvs scope in 
                             Printf.printf "AST has %d free variables \n" (IdentSet.cardinal fvs);
                             Printf.printf "scope has %d bound variables \n " (IdentSet.cardinal scope);
                             Printf.printf "There are %d free variables \n " (IdentSet.cardinal (IdentSet.diff fvs scope));
                             (if not (IdentSet.is_empty free) then perform (FreeVar free));
                             continue k ()
      | c, fvs -> (c, IdentSet.diff fvs scope)

   let new_scope alphas f name = let scope = Printf.printf "New scope: %s \n" name; IdentSet.of_list alphas in 
                                             Printf.printf "Added scope: %d bound variables \n" (IdentSet.cardinal scope);
                                             exec_in_current_scope scope f
                                             
  let empty = fun () -> IdentSet.empty
  let free_var var = IdentSet.singleton var
  let merge_free_vars fvs1 fvs2 = IdentSet.union fvs1 fvs2
  let check vars name =  Printf.printf "Checking: %s \n" name; if not (IdentSet.is_empty vars) then Printf.printf "called with %d free variables \n" (IdentSet.cardinal vars); perform (FreeVar vars)

  let scope_extrusion_check f = try f () with 
      effect FreeVar _, _ -> failwith "Scope Extrusion Check"

end