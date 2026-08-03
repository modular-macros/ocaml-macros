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

(* A primitive is one source declaration that each world incarnates at
   its own use sites: it has no address and no slot, and Translprim
   materialises every use inline -- so there is nothing to capture and
   no level to pin.  Primitives are level-neutral, like types and
   ordinary constructors.  A quoted same-unit primitive used to crash
   the compiler: the capture walk's no-address fallback put the
   primitive's unbound name in the macro's environment tuple
   (Bytegen.comp_expr: var id).  *)

external id : 'a -> 'a = "%identity"
external bump : int -> int = "%succint"

(* the crash shape: quoted, applied *)
macro m1 () = << id 1 >>

(* quoted as a value: the eta-expansion is embedded, closed *)
macro m2 () = << id >>

(* direct use in compile-time code: materialised in the static program *)
macro m3 () = Expr.int (id 3)

(* a primitive beside a REAL capture: skipping the primitive must not
   disturb the environment of the genuine one *)
let w = 10
macro m4 () = << id w + bump 0 >>

(* through a nested module *)
module N = struct external f : int -> int = "%succint" end
macro m5 () = << N.f 3 >>

(* through an open at a different level than the open's own *)
open N
macro m6 () = Expr.int (f 5)

(* level +1: a top-level quotation may use primitives, its own and the
   stdlib's.  (A level-0 expr value is inert -- it cannot be spliced --
   so the quotations are spliced where they are written.) *)
macro run (x : int expr) = x

let () =
  Printf.printf "%d %d %d %d %d %d %d %d\n"
    $(m1 ()) ($(m2 ()) 2) $(m3 ()) $(m4 ()) $(m5 ()) $(m6 ())
    $(run << id 7 >>) $(run << succ 7 >>)
