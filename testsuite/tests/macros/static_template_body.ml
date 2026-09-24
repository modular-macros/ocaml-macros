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

(* Static bindings in the bodies of TEMPLATE functors.

   A template functor is instantiated at each application, so a static
   in its body is evaluated at application time -- once per
   application -- and may read the parameter's static members.  It may
   not read the parameter's generators or run-time members
   (static_template_body_errors.ml).  The body's generators and
   splices may use it; it is evaluated before any of the body's
   splices, whatever the source order, as a unit's statics precede its
   emit call. *)

$let counter = ref 0

module type S = sig $val v : int  val w : int end

module F [X : S] = struct
  (* Per application: the two applications below see 1 and 2. *)
  $let stamp = incr counter; !counter
  (* From the parameter's static member. *)
  $let s = X.v + 1
  $gen m () = Expr.int (s * 10)
  let a = $(m ())
  let b = $(Expr.int (s + 100))
  let c = X.w
  let d = $(Expr.int stamp)
end

module A = F [struct $let v = 1  let w = 7 end]
module B = F [struct $let v = 2  let w = 8 end]

(* Hidden by an ascription. *)
module type R = sig val a : int end
module G [X : S] : R = struct
  $let s = X.v + 1
  let a = $(Expr.int s)
end
module H = G [struct $let v = 41  let w = 0 end]

(* Two parameters (applied through two bindings, as an application
   must be a whole right-hand side), and a nested template definition
   reading both. *)
module Two [X : S] [Y : S] = struct
  $let s = X.v + Y.v
  let a = $(Expr.int s)
end
module T1 = Two [struct $let v = 20  let w = 0 end]
module T = T1 [struct $let v = 22  let w = 0 end]

module Outer [X : S] = struct
  module Inner [Y : S] = struct
    $let s = X.v * Y.v
    let a = $(Expr.int s)
  end
end
module O = Outer [struct $let v = 6  let w = 0 end]
module I = O.Inner [struct $let v = 7  let w = 0 end]

(* Evaluation order: the static is evaluated by the record pass,
   before the body's splices run, although it follows the splice in
   the source. *)
$let trace = ref []
module Order [X : S] = struct
  let a = $(trace := "splice" :: !trace; Expr.int 0)
  $let _ = trace := "static" :: !trace
  let names = $(Expr.string (String.concat " " (List.rev !trace)))
end
module Q = Order [struct $let v = 0  let w = 0 end]

let () =
  Printf.printf "%d %d %d %d | %d %d %d %d | %d | %d %d | %s\n"
    A.a A.b A.c A.d B.a B.b B.c B.d H.a T.a I.a Q.names
