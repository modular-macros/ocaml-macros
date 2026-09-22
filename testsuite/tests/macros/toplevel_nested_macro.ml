(* TEST
 toplevel;
*)

(* A $gen nested in a module of a toplevel phrase used to be unusable
   as soon as it quoted any run-time name: its quotations were neither
   closed nor their roots pinned (both are done for DIRECT toplevel
   $gen items), so splicing its result failed with a nameless
   scope-extrusion error.  Batch always handled these.  The quoted
   roots are now pinned in the toplevel value table under their
   name/stamp keys, from the roots' lambda variables -- which also
   covers roots a sealed signature hides from the module's block. *)

(* structure-local root *)
module M = struct let a = 1 $gen m1 () = << a + 10 >> end;;
let x = $(M.m1 ());;

(* root from an earlier phrase *)
let b = 5;;
module M1 = struct $gen m () = << b + 1 >> end;;
let v1 = $(M1.m ());;

(* capture-the-binding: a later rebinding of [b], at a different type,
   must not affect the code the $gen returns *)
let b = "rebound";;
let v2 = $(M1.m ());;

(* enclosing-structure root, $gen under a nested module *)
module M3 = struct
  let a = 30
  module Inner = struct $gen m () = << a + 1 >> end
end;;
let v3 = $(M3.Inner.m ());;

(* structure-local module root, quoted through a projection *)
module M4 = struct
  module Inner = struct let c = 40 end
  let d = 2
  $gen m () = << Inner.c + d >>
end;;
let v4 = $(M4.m ());;

(* $gen rec group with mixed structure-local use *)
module M5 = struct
  let base = 50
  $gen rec even n =
    if n = 0 then << base >> else << ignore $(odd (n-1)); base + 1 >>
  and odd n =
    if n = 0 then << base + 2 >> else << ignore $(even (n-1)); base + 3 >>
end;;
let v5 = $(M5.even 2);;

(* the quoted root hidden by a sealed signature *)
module M6 : sig $gen m : unit -> int expr end =
  struct let hidden = 60 $gen m () = << hidden + 1 >> end;;
let v6 = $(M6.m ());;

(* structure-local root shadowed by a later item: the $gen keeps the
   binding it was typed against *)
module M7 = struct let a = 70 $gen m () = << a >> let a = 999 end;;
let v7 = $(M7.m ());;

(* include-bound root *)
module Src = struct let inc = 80 end;;
module M8 = struct include Src $gen m () = << inc + 1 >> end;;
let v8 = $(M8.m ());;

Printf.printf "%d %d %d %d %d %d %d %d %d\n" x v1 v2 v3 v4 v5 v6 v7 v8;;
