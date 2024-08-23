(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)


(* Extrusion with references *)
macro m () =
  let r = ref << 0 >> in
  let _ = << fun x -> $(r := <<x>> ; <<x>>) >> in
  !r

let y = $(m ())


(* Extrusion with exceptions *)
exception E of int expr

macro m2 () =
  try 
    let _ = << fun x -> $(raise (E <<x>>)) >> in << 0 >>
  with E x -> x

let z = $(m2 ())

(* Effects without extrusion *)
type _ Effect.t += R : int expr -> int expr Effect.t
                                    
macro m3 () =
  match << fun x -> $(Effect.perform (R <<x>>)) >> with
  | y -> y
  | effect R z, k -> Effect.Deep.continue k << $z + 1 >>

let w = $(m3 ()) 10
let () = assert (w = 11)
