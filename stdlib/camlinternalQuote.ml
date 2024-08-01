module Identifier =
struct
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
end
