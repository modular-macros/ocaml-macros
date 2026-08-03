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

(* Top-level [include F[V]]: a hidden application item followed by the
   include's usual field rebinding, so the include's items occupy
   positions exactly as a plain include's do.  The included VALUES
   read the instantiated fragment; the included MACRO is callable in a
   later splice by its short name.  Two includes check hygiene: the
   second's rebinds shadow the first's, and each instantiation has
   fresh state.  A literal argument exercises the D3 in-item binder
   under an include; a unit parameter the argument-free form. *)

let scale = 3
module F[X : sig val base : int macro gen : unit -> int expr end] = struct
  let v = X.base * scale
  macro m () = << v + 1 >>
  let w = $(m ()) + $(X.gen ())
  let r = ref v
end

module V = struct let base = 10 macro gen () = << 4 >> end

include F[V]
let y1 = $(m ())
module Keep = struct let v = v let w = w end

include F[struct let base = 100 macro gen () = << 7 >> end]
let y2 = $(m ())

(* State: the second include's [r], not the first's. *)
let () = incr r

module G[] = struct macro mg () = << 5 >> let u = $(mg ()) end
include G[]
let y3 = $(mg ())

let () =
  Printf.printf "%d %d %d %d %d %d %d %d %d\n"
    Keep.v Keep.w v w y1 y2 !r u y3
