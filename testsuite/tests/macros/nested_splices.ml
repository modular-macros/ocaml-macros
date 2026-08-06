(* TEST
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 run;
 check-program-output;
*)

(* A top-level splice is collected while the structure item enclosing it is
   translated, so that the macros in scope there are bound in the static
   program.  These cases all put a splice inside a nested structure; the ones
   that bind the macro in that same structure used to collect the splice a
   second time, at a point where the macro was not in scope, and died with
   "Bytegen.comp_expr: var m". *)

macro outer () = << 1 >>

(* Macro and splice in the same nested module. *)
module A = struct
  macro m () = << 2 >>
  let v = $(m ())
end

(* Two levels of nesting, macro bound innermost. *)
module B = struct
  module C = struct
    macro m () = << 3 >>
    let v = $(m ())
  end
end

(* Macro bound outside the nested module. *)
module D = struct
  let v = $(outer ())
end

(* Macro bound in a nested module, spliced outside it. *)
module E = struct
  macro m () = << 4 >>
end
let e = $(E.m ())

(* A recursive module. *)
module rec F : sig val v : int end = struct
  let v = $(outer ())
end

(* A functor body is translated under an abstraction, so its splice is
   collected outside the functor and evaluated whether or not the functor is
   ever applied. *)
module type S = sig end
module G (X : S) = struct
  let v = $(outer ())
end
module H = G (struct end)

(* Never applied. *)
module Unused (X : S) = struct
  let v = $(outer ())
end

(* A splice in a functor argument. *)
module type T = sig val v : int end
module I (X : T) = struct let v = X.v end
module J = I (struct let v = $(outer ()) end)

let () =
  assert (A.v = 2);
  assert (B.C.v = 3);
  assert (D.v = 1);
  assert (e = 4);
  assert (F.v = 1);
  assert (H.v = 1);
  assert (J.v = 1);
  print_endline "ok"
