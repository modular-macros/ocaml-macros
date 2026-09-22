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

(* A pack passed to a DEPENDENT package parameter, [(module M : S) ->
   'a M.t -> ...].  Typecore types that argument by a different route
   from an ordinary pack: it folds the pack's own annotation into the
   module expression as an ascription.  That ascription used to be
   matched before [type_package] opened the relativity window, so it
   compared absolutely and a module of static bindings failed against
   a plain-[val] package type -- while the SAME pack written without
   the annotation matched (PACK-MATCHING.md).  The annotation is the
   package's type, not an ascription the user wrote inside the pack,
   so it is now applied as part of the package's own coercion.

   The coercion from the annotation on to the expected package type
   relates two package types already read at the package's level, and
   so is absolute: applying the relativity to it as well would shift
   twice. *)

module type S = sig
  type _ t
  val get : 'a. 'a t -> int
end

$gen use : (module M : S) -> 'a M.t -> int expr =
  fun (module M : S) x -> Expr.int (M.get x)

(* The reported shape: a parameterized abstract type member with a
   member polymorphic over its parameter. *)
module M = struct
  type 'a t = 'a list
  $let get : 'a. 'a t -> int = fun xs -> List.length xs
end

(* The same, without the annotation: the route that always worked. *)
let a = $(use (module M : S) [1; 2; 3])
let b = $(use (module M) [1; 2; 3; 4])

(* A monomorphic member fails the same way when the parameter is
   dependent, so the trigger is the parameter, not the polymorphism. *)
module type Sv = sig type _ t val v : int end
$gen usev : (module M : Sv) -> 'a M.t -> int expr =
  fun (module M : Sv) _x -> Expr.int M.v
module Mv = struct type 'a t = 'a list  $let v = 41 end
let c = $(usev (module Mv : Sv) [1])

(* The GADT form. *)
module Mg = struct
  type _ t = T : int t
  $let get : type a. a t -> int = fun T -> 7
end
let d = $(use (module Mg : S) Mg.T)

let () = Printf.printf "%d %d %d %d\n" a b c d
