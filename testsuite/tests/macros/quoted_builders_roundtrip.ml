(* TEST
 set MACOCAML_QUOTED_BUILDERS = "1";
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

(* The degenerate exercise of the template-functor fragment machinery
   (3B-SCHEME.md, 3(b)(ii) step 2): this module's run-time term is built
   afresh -- every binder renamed -- by evaluating a quoted module
   builder in the static program, and its macro object is emitted by the
   static program from a builder of its term, as a template
   application's fragments will be.  The module is future-closed: no
   splice's code references a term binder by stamp -- returned code here
   is closed -- which is the builder precondition template bodies
   satisfy by closure conversion.  Splices quoting enclosing binders
   (power.ml's shape) are NOT builder-compatible until the thunk
   convention of step 3 passes renamed binder values to holes. *)

macro m () = << 21 >>

let v = $(m ()) * 2

module N = struct
  macro g () = << 5 >>
  let w = $(g ()) + 1
end

include struct
  macro h () = << 4 >>
  let u = $(h ())
end

let arr = [| $( << 10 >> ); $( << 20 >> ) |]

let f (a, b) = a * b + $( << 7 >> )

module F (X : sig end) = struct
  let body_splice = $( << 3 >> )
end
module A = F (struct end)

let s = (match "k" ^ "ey" with
         | "key" -> $( << 8 >> ) | "no" -> 1 | _ -> 0)

let () =
  Printf.printf "v=%d w=%d u=%d arr=%d,%d f=%d A=%d s=%d\n"
    v N.w u arr.(0) arr.(1) (f (2, 3)) A.body_splice s
