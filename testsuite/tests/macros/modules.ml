(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)

(* macro m () =
 *   let module M = struct
 *       let m () = 3
 *   end in
 *   <<()>> *)


module F1 = struct
  macro m1 () = << 3 >>
end
let x1 = $(F1.m1 ())

module F2 = struct
  macro m2 () = << 3 >>
  let x2 = $(m2 ())
end
            
macro m3 () = << 3 >>
module F3 = struct
  let x3 = $(m3 ())
end
