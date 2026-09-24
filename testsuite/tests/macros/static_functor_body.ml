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

(* Static bindings in the bodies of PLAIN functors (mixed or not).

   A plain functor's compile-world meaning is a block built without
   the argument, once, so a static in its body is evaluated when that
   block is built -- when the macros object is loaded -- and not at
   application time: every application sees the same value.  It may
   not read the parameter's values (the level check refuses them,
   static_functor_body_errors.ml), only its types, so it cannot depend
   on the application; what it is for is local definitions shared by
   the body's generators, whose environments are per-application. *)

$let counter = ref 0

module F (X : sig type t val v : int end) = struct
  (* Evaluated once: both applications below see 1. *)
  $let stamp = incr counter; !counter
  (* A parameter TYPE is fine -- types carry no level. *)
  $let empty : X.t list = []
  $gen m () = << $(Expr.int stamp) * 100 + X.v >>
  $gen n () = Expr.int (List.length empty)
  module Inner = struct
    $gen k () = Expr.int (stamp + 10)
  end
end

module A = F (struct type t = int let v = 1 end)
module B = F (struct type t = string let v = 2 end)

(* A functor whose only compile-time content is a static: its
   application carries the static, read through the application. *)
module G (X : sig end) = struct $let g = 5 end
module C = G (struct end)

(* A plain functor's parameter may not declare a static
   (static_functor_param.ml), but the test is on the parameter's type:
   an argument that has one is fine at a signature leaving it out. *)
module type T = sig val w : int end
module L (X : T) = struct let w = X.w end
module M = L (struct let w = 7  $let v = 1 end)

let () =
  Printf.printf "%d %d %d %d %d %d %d\n"
    $(A.m ()) $(B.m ()) $(A.n ()) $(A.Inner.k ()) $(B.Inner.k ())
    $(Expr.int C.g) M.w
