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

(* [open F$(V)] is compiled as [include F$(V)] is: a hidden application
   item followed by the rebinding of the result's fields. *)

module F$(X : sig val v : int end) = struct
  let w = X.v + 1
  $let[..] g () = << w >>
end
module C$(X : sig val a : int end)$(Y : sig val b : int end) = struct
  let v = X.a * 10 + Y.b
end

open F$(struct let v = 1 end)
let () = Printf.printf "%d %d\n" w $(g ())

module H (P : sig val v : int end) = struct open F$(P) let z = w end
module T$(P : sig val v : int end) = struct open F$(P) let z = w * 2 end
module H1 = H (struct let v = 1 end)
module T1 = T$(struct let v = 1 end)
let () = Printf.printf "%d %d\n" H1.z T1.z

open C$(struct let a = 1 end)$(struct let b = 2 end)
let () = Printf.printf "%d\n" v
