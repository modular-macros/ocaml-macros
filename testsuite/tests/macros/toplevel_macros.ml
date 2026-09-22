(* TEST
 toplevel;
*)

(* Macros in the toplevel.  Unlike batch compilation, which builds a separate
   static program, the toplevel evaluates the compile-time part of a phrase in
   process, against its own symbol table and value table. *)

$gen rec pow n x = if n = 0 then << 1 >> else << $x * $(pow (n-1) x) >>
;;

(* A $gen defined by an earlier phrase. *)
let cube y = $(pow 3 << y >>)
;;

cube 3
;;

(* Splice indices are numbered per phrase, so a second phrase with splices
   must start again from zero. *)
$gen succ_of x = << $x + 1 >>
;;

let a = $(succ_of << 10 >>)
;;

let b = $(succ_of << 20 >>)
;;

(a, b)
;;

(* Macro and splice in the same phrase. *)
$gen seven () = << 7 >> let v = $(seven ())
;;

(* Macro and splice in the same nested module: the splice's thunk is built
   where the $gen is in scope and carried out through the splice array --
   bound by the static program rather than the toplevel table -- then
   applied at the hole when the phrase's run-time term is constructed. *)
module F = struct
  $gen five () = << 5 >>
  let x = $(five ())
end
;;

F.x
;;

(* A splice may have a compile-time effect, which happens when the phrase is
   processed. *)
$gen noisy () = (print_endline "[compile-time]"; << 3 >>)
;;

let n = $(noisy ())
;;
