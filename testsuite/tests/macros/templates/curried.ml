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

(* Curried templates: the outer level's record is the inner level's
   component function, and a partial application's run-time slot is
   the extended environment. *)

let scale = 2
module F$(X : sig val a : int  $val[..] g : unit -> int expr end)
        $(Y : sig val b : int  $val[..] h : int expr -> int expr end) = struct
  let v = (X.a + Y.b) * scale
  $let[..] k () = << $(Y.h (X.g ())) + v >>
  let w = $(k ())
end

module V = struct let a = 5  $let[..] g () = << 3 >> end
module W = struct let b = 7  $let[..] h c = << $c * 10 >> end

module H = F$(V)          (* partial: record = inner function; slot = context *)
module M = H$(W)

let y = $(M.k ())
let () = Printf.printf "%d %d %d\n" M.v M.w y
