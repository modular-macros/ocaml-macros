(* Helper unit for toplevel_template_import.ml: a batch-compiled
   template functor, applied from the toplevel. *)

module type S = sig val base : int macro gen : unit -> int expr end

let scale = 3

module F[X : S] = struct
  let v = X.base * scale
  macro m () = << v + 1 >>
  let w = $(m ()) + $(X.gen ())
end
