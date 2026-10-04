(* Helper unit for toplevel_import.ml: a batch-compiled
   template, applied from the toplevel. *)

module type S = sig val base : int $val[..] gen : unit -> int expr end

let scale = 3

module F$(X : S) = struct
  let v = X.base * scale
  $let[..] m () = << v + 1 >>
  let w = $(m ()) + $(X.gen ())
end
