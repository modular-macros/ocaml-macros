(* TEST
 setup-ocamlc.byte-build-env;
 all_modules = "open_struct_items.ml";
 ocamlc.byte;
 run;
 check-program-output;
*)

(* [open struct ... end] at the compile-time stage.  Its payload used to be
   dummied whatever it contained, so a macro it bound resolved to the dummy
   and the static program applied unit to nothing (SIGSEGV), while a
   template application under it, and a body binder quoted through it, were
   invisible to the stage -1 scans and reached Bytegen unbound.  An open is
   now treated as the include arm treats its payload: dummied only when it
   brings in no compile-time content, and hoisted when it does, so the
   binders it introduces span the continuation. *)

(* A macro bound by an open. *)
open struct macro m () = << 21 >> end
let a = $(m ()) * 2

(* A macro arriving through an open of a mixed functor's application. *)
module F (X : sig end) = struct macro mm = fun () -> << 3 >> end
open F(struct end)
let b = $(mm ())

(* A template application among an open's items: its fragment slot is read
   from the continuation, not from inside the payload. *)
module T [X : sig val base : int end] = struct
  let v = $( << X.base * 2 >> )
end
open struct module M = T[struct let base = 10 end] end
let c = M.v

(* A name bound by an open inside a template functor body, quoted: a body
   binder, not a captured root of the functor's environment. *)
module G [X : sig end] = struct
  open struct let iv = 5 end
  let v = $( << iv >> )
end
module N = G[struct end]
let d = N.v

let () = Printf.printf "%d %d %d %d\n" a b c d
