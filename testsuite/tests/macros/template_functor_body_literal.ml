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

(* Literal (non-path) arguments and functors in applications INSIDE
   template functor bodies (BODY-MACRO-LIFT.md 7.5): a literal
   argument is an anonymous body module -- fragment side an in-item
   binding at the application's position, freshened per OUTER
   application, its identity delivered at the pseudo-thunk's hole;
   chain side an in-item stage -1 binding before the record call, so
   its macros capture x_macros.  Nothing references a fragment binder
   by baked stamp. *)

(* (a) 7.5's example: the literal's value depends on X; its macro h
   is literal-defined; M0.w's value checks the delivery. *)
module G[Y : sig val b : int  macro h : int expr -> int expr end] =
struct
  let v = Y.b * 2
  let w = $(Y.h << v >>)
end
module F[X : sig val base : int end] = struct
  module M0 = G[struct let b = X.base + 1
                       macro h c = << $c + 10 >> end]
  let s = M0.w
end
module A0 = F[struct let base = 5 end]

(* (b)+(c) two outer applications with different X keep the literal's
   instantiations (values and state) apart; the literal's macro calls
   X's macros; the instantiated inner macro is used from body splices
   and from later top-level splices through the record chain. *)
module G2[Y : sig val b : int  macro h : int expr -> int expr end] = struct
  let v = Y.b * 3
  let w = $(Y.h << v >>)
  let r = ref v
  macro k () = << v + w >>
end
module F2[X : sig val base : int  macro gen : unit -> int expr end] = struct
  module M0 = G2[struct
      let b = X.base + 1
      macro h c = << $c + $(X.gen ()) >>
    end]
  let s = M0.w
  let t = $(M0.k ())
end
module V1 = struct let base = 10  macro gen () = << 100 >> end
module V2 = struct let base = 20  macro gen () = << 1000 >> end
module A = F2[V1]
module B = F2[V2]
let () = A.M0.r := !A.M0.r + 1
let () = B.M0.r := !B.M0.r + 1000
let y = $(A.M0.k ()) + $(B.M0.k ())

(* (d) the functor position lifts by the same move: an immediate
   anonymous template functor, applied to a literal argument (M0) and
   to a body module (M1). *)
module F3[X : sig val base : int  macro gen : unit -> int expr end] = struct
  module M0 =
    (functor [Z : sig val s : int  macro zg : unit -> int expr end] ->
       struct
         let t = Z.s + X.base
         let u = $(Z.zg ()) + $(X.gen ())
         macro k () = << t >>
       end)[struct let s = 3  macro zg () = << 7 >> end]
  module Q = struct let s = 40  macro zg () = << 11 >> end
  module M1 =
    (functor [Z : sig val s : int  macro zg : unit -> int expr end] ->
       struct let t = Z.s * 2 + X.base  let u = $(Z.zg ()) end)[Q]
  let a = M0.t
  let b = $(M0.k ())
end
module C = F3[struct let base = 5  macro gen () = << 50 >> end]
module D = F3[struct let base = 600  macro gen () = << 70 >> end]
let y3 = $(C.M0.k ()) + $(D.M0.k ())

let () =
  Printf.printf "%d %d\n" A0.M0.v A0.s;
  Printf.printf "%d %d %d %d %d %d %d %d %d\n"
    A.M0.v A.s A.t B.M0.v B.s B.t !A.M0.r !B.M0.r y;
  Printf.printf "%d %d %d %d %d %d %d %d\n"
    C.M0.t C.M0.u C.a C.b C.M1.t C.M1.u D.M0.t y3
