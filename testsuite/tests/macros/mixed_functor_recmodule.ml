(* TEST
 setup-ocamlc.byte-build-env;
 all_modules = "mixed_functor_recmodule.ml";
 ocamlc.byte;
 run;
 check-program-output;
*)

(* A recursive module inside a MIXED functor's body.  The compile-time
   meaning of a mixed functor is its argument-blind shared block, so
   nothing in the body that depends on the parameter may be translated
   there.  The plain module binding had that guard; the recursive one did
   not, and [module rec A : S = B (X)] reached Bytegen with X unbound. *)

module B (Y : sig val v : int end) = struct let w () = Y.v end

module Mixed (X : sig val v : int end) = struct
  macro mm () = << 5 >>
  module rec A : sig val w : unit -> int end = B (X)
end

module M = Mixed (struct let v = 7 end)

let () = Printf.printf "%d\n" ($(M.mm ()) + M.A.w ())
