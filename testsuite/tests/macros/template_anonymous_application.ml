(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)

(* [module _ = F[V]]: an anonymous template application evaluates --
   the compile-time component call and the run-time result's
   initialisation effects both happen -- but binds no name and
   occupies no position in the enclosing block. *)

module F[X : sig val v : int end] = struct
  let () = Printf.printf "init:%d " X.v
  let w = X.v * 2
end

module V = struct let v = 3 end

module _ = F[V]

(* Positions after the anonymous item are unshifted. *)
let after = 1
module Named = F[V]

let () = Printf.printf "after:%d named:%d\n" after Named.w
