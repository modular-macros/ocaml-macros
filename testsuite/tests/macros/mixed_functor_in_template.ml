(* TEST
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* Mixed functors and template functors, interleaved
   (SPLITTING-FUNCTORS.md residual lifts S2, S3 and B6): a mixed
   functor DEFINED in a template body, its macros calling the
   template parameter's macros, applied downstream through the
   record; a mixed functor APPLIED inside a template body with an
   instantiation-dependent argument; the S2+S3 combination with the
   mixed functor's own body splice, evaluating once per OUTER
   instantiation; a curried in-template mixed functor partially
   applied through the record; and macro-bearing BODY-LOCAL template
   arguments, including one whose macro captures the enclosing
   parameter. *)

(* S2: defined in a template body, macros reaching T's macros. *)
module Outer[T : sig val t : int macro tg : unit -> int expr end] = struct
  let base = T.t
  module F (X : sig val v : int end) = struct
    let w = X.v + base
    macro m () = << X.v * 100 + w + $(T.tg ()) >>
  end
end
module W = struct let t = 7 macro tg () = << 1000 >> end
module O = Outer[W]
module N = O.F (struct let v = 2 end)
let () = Printf.printf "%d %d\n" $(N.m ()) N.w

(* S3: applied inside a template body, argument depending on T. *)
module F0 (X : sig val v : int end) = struct
  let w = X.v * 2
  macro m () = << X.v + w >>
end
module App[T : sig val t : int end] = struct
  module N = F0 (struct let v = T.t end)
  let r = $(N.m ()) + N.w
end
module A1 = App[struct let t = 3 end]
module A2 = App[struct let t = 5 end]
let () = Printf.printf "%d %d\n" A1.r A2.r

(* S2+S3 with the mixed functor's own body splice: once per OUTER
   instantiation, per the template-copying reading of "definition
   point". *)
module Both[T : sig val t : int end] = struct
  module G (X : sig val v : int end) = struct
    macro g () = << X.v + T.t >>
    let sp = $(g ())
  end
  module M = G (struct let v = 100 end)
  let r = M.sp
end
module B1 = Both[struct let t = 1 end]
module B2 = Both[struct let t = 2 end]
let () = Printf.printf "%d %d\n" B1.r B2.r

(* Curried S2, partially applied through the record. *)
module Cur[T : sig val t : int end] = struct
  module F (X : sig val a : int end) (Y : sig val b : int end) = struct
    macro m () = << X.a * 100 + Y.b * 10 + T.t >>
  end
end
module OC = Cur[struct let t = 5 end]
module PC = OC.F (struct let a = 1 end)
module NC = PC (struct let b = 2 end)
let () = Printf.printf "%d\n" $(NC.m ())

(* B6: macro-bearing body-local arguments; the second macro captures
   the enclosing parameter through its environment. *)
module T6[Y : sig val u : int macro g : unit -> int expr end] = struct
  let r = Y.u * 100 + $( Y.g () )
end
module H (P : sig val p : int end) = struct
  module W1 = struct let u = P.p macro g () = << 50 >> end
  module W2 = struct let u = P.p macro g () = << u + 7 >> end
  module Z1 = T6[W1]
  module Z2 = T6[W2]
  let s = Z1.r + Z2.r
end
module H1 = H (struct let p = 1 end)
module H3 = H (struct let p = 3 end)
let () = Printf.printf "%d %d\n" H1.s H3.s
