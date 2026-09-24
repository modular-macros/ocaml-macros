(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)

(* A class beside a $gen.  A class has no compile-time content, but it
   occupies a run-time position, so at the compile-time stage it must get a
   dummy slot -- previously it was appended to the block's fields with no
   binding, and translating the $gen block crashed on the unbound class
   identifier. *)

class c = object method x = 41 end

$gen m () = << 2 >>

let v = (new c)#x + $(m ())

(* A class defined after the $gen, and a splice after both. *)

class d = object method y = 100 end

let w = (new d)#y + $(m ())

let () = Printf.printf "v = %d\nw = %d\n" v w
