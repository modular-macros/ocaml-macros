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

(* Curried template functors (BODY-MACRO-LIFT.md, Example D): the
   inner component function is the outer record; the partial
   application's run-time slot is the extended context; projection
   paths compose one level per parameter. *)

let scale = 2
module F[X : sig val a : int  $gen g : unit -> int expr end]
        [Y : sig val b : int  $gen h : int expr -> int expr end] = struct
  let v = (X.a + Y.b) * scale
  $gen k () = << $(Y.h (X.g ())) + v >>
  let w = $(k ())
end

module V = struct let a = 5  $gen g () = << 3 >> end
module W = struct let b = 7  $gen h c = << $c * 10 >> end

module H = F[V]          (* partial: record = inner function; slot = context *)
module M = H[W]

let y = $(M.k ())
let () = Printf.printf "%d %d %d\n" M.v M.w y
