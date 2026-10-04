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

(* Applications in the unit of the definition: the body's splices run
   once per application, with values and state of their own; a later
   splice calls an instantiated generator; the body's splices call the
   argument's generators and the body's. *)

let scale = 3
module F$(X : sig val base : int $val[..] gen : unit -> int expr end) = struct
  let v = X.base * scale
  $let[..] m () = << v + 1 >>
  let w = $(m ()) + $(X.gen ())
  let r = ref v
end

module V = struct
  let base = 10
  $let[..] gen () = << 4 >>
end
module V2 = struct
  let base = 100
  $let[..] gen () = << 7 >>
end

module M = F$(V)
module M2 = F$(V2)

(* Fresh state per instantiation. *)
let () = M.r := !M.r + 1

(* The instantiated macros, called like anyone else's. *)
let y = $(M.m ()) + $(M2.m ())

(* A unit parameter applies as G$(). *)
module G$() = struct let u = $( << 5 >> ) end
module MG = G$()

let () =
  Printf.printf "%d %d %d %d %d %d %d %d\n"
    M.v M.w M2.v M2.w !M.r !M2.r y MG.u
