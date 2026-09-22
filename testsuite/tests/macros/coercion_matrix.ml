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

(* The coercion matrix for level -1-bearing modules
   (frex bug 3): narrowing and reordering signatures over
   template functors, mixed functors, and their members, applied at
   every site the translator meets -- inline result ascriptions,
   stage 0 alias ascriptions, application-result ascriptions,
   curried aliases, and template-parameter (x_macros) narrowing --
   and consumed on both faces: compile-time projection and splicing,
   AND run-time reads through the code-coerced fragment (positions
   shift when a $gen before a value is hidden).  The face-tagged
   functor coercions (Fcf_plain/mixed/template) drive the stage -1
   application: shared blocks take the result half, component
   functions coerce the (record, dyn) pair with the fragment coerced
   as code, and stage 0 template slots (env tuples) pass through
   untouched.  The cross-unit .mli forms live in the tests
   repository (coercion_matrix_mli). *)

module type S = sig type t $gen id : t expr -> t expr end
module MI = struct type t = int $gen id x = x end

(* G1: hide a $gen declared BEFORE a run-time value: narrowed
   positions shift, and the value is read at RUN time through the
   coerced fragment (the dyn half, as code). *)
module F1 [A : S] : sig val r : int val s : int end = struct
  $gen hidden () = << 0 >>
  let r = 10
  let s = 20
end
module N1 = F1[MI]
let () = Printf.printf "%d %d\n" N1.r N1.s

(* G2: REORDER macros and values, plus a hidden tail. *)
module F2 [A : S] : sig
  val r : int
  $gen go : A.t expr -> A.t expr
end = struct
  $gen go x = A.id x
  let r = 5
  $gen dead () = << 9 >>
end
module N2 = F2[MI]
let () = Printf.printf "%d %d\n" N2.r $( N2.go << 41 >> )

(* G3: STAGE 0 alias ascription of a template functor, then applied:
   the run slot (env tuple) passes through the coercion unreshaped. *)
module F3 [A : S] = struct
  $gen go x = A.id x
  $gen extra () = << 1 >>
  let v = 7
end
module F3n : [A : S] sig $gen go : A.t expr -> A.t expr val v : int end =
  F3
module N3 = F3n[MI]
let () = Printf.printf "%d %d\n" N3.v $( N3.go << 6 >> )

(* G4: curried template narrowed at the outer type; partial applied. *)
module F4 [A : S] [B : S] = struct
  $gen go x = B.id x
  $gen dead () = << 0 >>
end
module F4n : [A2 : S] [B2 : S] sig $gen go : B2.t expr -> B2.t expr end =
  F4
module P4 = F4n[MI]
module N4 = P4[MI]
let () = Printf.printf "%d\n" $( N4.go << 21 >> )

(* G5: a MIXED functor narrowed by an alias ascription and by an
   application-result ascription; run and compile faces both read. *)
module FM (X : sig val v : int end) = struct
  let w = X.v * 2
  $gen m () = << X.v + w >>
  $gen deadm () = << 0 >>
  let deadv = 99
end
module FMn : (X : sig val v : int end) ->
  sig val w : int $gen m : unit -> int expr end = FM
module NM = FMn (struct let v = 3 end)
module NM2 : sig $gen m : unit -> int expr end = FM (struct let v = 4 end)
let () = Printf.printf "%d %d %d\n" NM.w $( NM.m () ) $( NM2.m () )

(* G7: a template member inside a mixed functor, applied THROUGH the
   mixed functor's narrowed result. *)
module FT (X : sig val v : int end) : sig
  module T : [Y : S] sig $gen tm : unit -> int expr end
end = struct
  let base = X.v
  module T [Y : S] = struct
    $gen tm () = << base * 10 >>
    $gen tdead () = << 0 >>
  end
  $gen fdead () = << 0 >>
end
module NT = FT (struct let v = 6 end)
module TI = NT.T[MI]
let () = Printf.printf "%d\n" $( TI.tm () )

(* G8: x_macros narrowing -- the template PARAMETER's signature hides
   the provided module's extra members (the cc_arg path). *)
module Rich = struct
  type t = int
  $gen id x = x
  $gen extra () = << 1 >>
  let unused = 0
end
module F8 [A : S] = struct
  $gen go x = A.id x
end
module N8 = F8[Rich]
let () = Printf.printf "%d\n" $( N8.go << 8 >> )
