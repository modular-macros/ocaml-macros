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

(* The code that a generator builds may mention the run-time
   constructors in whose scope the generator is defined, as it may
   mention values. *)

exception Failed of int
type t = ..
type t += A | B of int

$let[..] guarded body = << try $body with Failed n -> n >>
$let[..] rank x = << match $x with A -> 1 | B n -> n | _ -> 0 >>
$let[..] slot () =
  << Obj.Extension_constructor.name [%extension_constructor Failed] >>

let fail n = raise (Failed n)

module F (X : sig val base : int end) = struct
  exception Local of int
  $let[..] guarded body = << try $body with Local n -> n + X.base >>
  let fail n = raise (Local n)
end
module FA = F (struct let base = 10 end)

(* Quoted code may declare constructors of its own. *)

$let[..] local () =
  << let exception Inner of int in try raise (Inner 5) with Inner n -> n >>

(* So may the code of a splice. *)

let spliced = try $(<< raise (Failed 6) >>) with Failed n -> n

let () =
  Printf.printf "%d %d %d %s %d %d %d\n"
    $(guarded << fail 1 >>) $(rank << A >>) $(rank << B 2 >>) $(slot ())
    $(FA.guarded << FA.fail 4 >>) $(local ()) spliced
