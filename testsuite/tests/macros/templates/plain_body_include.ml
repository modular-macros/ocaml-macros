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

(* Template applications under an include inside a functor body: an
   included literal structure's applications are items one level down,
   and a direct [include F$(Y)] is an application item with a hidden
   binder.  The compile-time part runs once per unit, at the
   definition, and the fragment per application. *)

module F$(X : sig val base : int end) = struct
  let v = X.base * 2
  let w = $( << X.base + 1 >> )
end

(* Include of a literal structure containing an application. *)
module H (Y : sig val base : int end) = struct
  include struct module M = F$(Y) end
  let s = M.v + M.w
end

module A = H (struct let base = 10 end)
module B = H (struct let base = 100 end)

(* Direct include of an application: the included values read the
   fragment's block, per application through the pair. *)
module H2 (Y : sig val base : int end) = struct
  include F$(Y)
  let s = v + w
end

module C = H2 (struct let base = 7 end)
module D = H2 (struct let base = 30 end)

let () = Printf.printf "%d %d %d %d\n" A.s B.s C.s D.s
