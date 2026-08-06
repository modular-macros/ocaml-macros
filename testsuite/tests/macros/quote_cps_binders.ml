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

(* A compile-time CPS helper that builds a quotation introducing fresh
   binders and hands quoted references to them to its continuation,
   whose output also references the binder of an ENCLOSING generated
   let rec.  Found porting AllegrOCaml (every generator destructuring
   its recursion state this way): the native-context static program
   died of SIGSEGV while byte was fine.  The cause was the
   calling-convention mismatch pinned by macro_tupled_param.ml, reached
   through an INNER tuple-pattern closure rather than the macro's own
   parameter: before the compile-time part was forced to bytecode
   translation (27d70a81eb), the continuation got the tupled convention
   under the native driver, Bytegen compiled it as curried, and the
   one-tuple call left a partial application where an expr pair was
   read.  Bracketed by hand: crashes at 9e5dcf2498, clean at
   f101a2cf91. *)

macro rec_pair2 : unit -> int expr = fun () ->
  let split (cp : (int * int) expr) (k : int expr * int expr -> int expr) =
    << let a, b = $cp in $(k (<< a >>, << b >>)) >>
  in
  << let rec go x =
       $(split << x >> (fun (a, b) ->
             << if $a = 0 then $b else go ($a - 1, $b + 1) >>))
     in
     go (5, 0) >>

let () = Printf.printf "%d\n" $(rec_pair2 ())
