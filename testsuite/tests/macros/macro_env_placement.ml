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

(* Where the macro SITS, and recursion, for environments within one unit.
   "Same module" for capture means the same compilation unit at any
   depth, so a macro in a nested module must capture an enclosing
   module's binding -- the shape that failed with
   "var a_274" before the environments landed. *)

let top = 1

module A = struct
  let a = 10

  module B = struct
    let b = 100

    module C = struct
      let c = 1000

      (* Captures at three enclosing depths plus the unit's top level. *)
      macro deep () = << top + a + b + c >>
    end

    (* An enclosing level's macro calling a deeper one. *)
    macro from_b () = << b * 2 + $(C.deep ()) >>
  end

  macro from_a () = << a * 3 + $(B.from_b ()) >>
end

let p1 = $(A.B.C.deep ())
let p2 = $(A.B.from_b ())
let p3 = $(A.from_a ())

(* A macro of a nested module used inside the SAME nested module. *)

module SelfUse = struct
  let base = 6
  macro g () = << base * 7 >>
  let inside = $(g ())
end

let p4 = SelfUse.inside

(* An anonymous-signature nested module inside a nested module, with the
   macro at the bottom and the captured name at the top. *)

module Wrap = struct
  let w = 4
  module Mid = struct
    module Bottom = struct
      macro g () = << w * 25 >>
    end
  end
end

let p5 = $(Wrap.Mid.Bottom.g ())

(* --- Recursion --------------------------------------------------- *)

(* A [macro rec] group in a nested module capturing an enclosing
   binding. *)

module R = struct
  let unit_step = 2
  macro rec down n =
    if n = 0 then << 0 >> else << unit_step + $(down (n - 1)) >>
end

let r1 = $(R.down 5)

(* A group in which only ONE member captures: the shared environment is
   the union, so the member that captures nothing still has to agree on
   the layout. *)

let only_a_captures = 9

macro rec ma n = if n = 0 then << only_a_captures >> else << $(mb (n - 1)) >>
and mb n = if n = 0 then << 0 >> else << 1 + $(ma (n - 1)) >>

let r2 = $(ma 4)
let r3 = $(mb 3)

(* A group whose members capture DIFFERENT bindings: the union has to
   place both, and each member projects at the right index. *)

let for_x = 30
let for_y = 500

macro rec mx n = if n = 0 then << for_x >> else << $(my (n - 1)) >>
and my n = if n = 0 then << for_y >> else << $(mx (n - 1)) >>

let r4 = $(mx 0)
let r5 = $(mx 1)
let r6 = $(my 0)
let r7 = $(my 1)

(* A self-recursive macro that captures, driven deep enough that the
   environment is threaded through many recursive calls. *)

let one = 1

macro rec count n = if n = 0 then << 0 >> else << one + $(count (n - 1)) >>

let r8 = $(count 40)

(* A recursive group reached through an alias of its module. *)

module AliasR = R

let r9 = $(AliasR.down 3)

(* --- Functor bodies ---------------------------------------------- *)

(* A [macro rec] group inside a mixed functor body, capturing the
   parameter, with the environment differing per application. *)

module F (X : sig val v : int end) = struct
  macro rec pow n = if n = 0 then << 1 >> else << X.v * $(pow (n - 1)) >>
  macro plain () = << X.v + top >>
end

module FA = F (struct let v = 3 end)
module FB = F (struct let v = 5 end)

let s1 = $(FA.pow 3)
let s2 = $(FB.pow 2)
let s3 = $(FA.plain ())
let s4 = $(FB.plain ())

(* A nested module inside a template functor body, whose macro captures
   both the template parameter and the unit's top level. *)

module T [Y : sig val u : int end] = struct
  let scaled = Y.u * 10
  module Sub = struct
    macro g () = << scaled + Y.u + top >>
  end
end

module TA = T [struct let u = 2 end]
module TB = T [struct let u = 7 end]

let s5 = $(TA.Sub.g ())
let s6 = $(TB.Sub.g ())

let () =
  Printf.printf "depth: %d %d %d\n" p1 p2 p3;
  Printf.printf "self: %d\n" p4;
  Printf.printf "bottom: %d\n" p5;
  Printf.printf "rec-nested: %d\n" r1;
  Printf.printf "rec-union: %d %d\n" r2 r3;
  Printf.printf "rec-different: %d %d %d %d\n" r4 r5 r6 r7;
  Printf.printf "rec-deep: %d\n" r8;
  Printf.printf "rec-alias: %d\n" r9;
  Printf.printf "functor: %d %d %d %d\n" s1 s2 s3 s4;
  Printf.printf "template: %d %d\n" s5 s6
