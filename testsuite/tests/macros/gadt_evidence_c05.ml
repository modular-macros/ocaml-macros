(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* C05: the equation escapes through an exception; the raise discards
   the match that justified it.  Typing admits the program now that the
   equation is usable in the nested quotation; the evidence catches it. *)

type (_, _) eql = Refl : ('a, 'a) eql

$gen c05 : type a b. (a, b) eql expr -> (a -> b) expr =
  fun r ->
    let exception E of b expr in
    << fun x -> $(try << match $r with Refl -> $(raise (E << x >>)) >>
                  with E y -> y) >>

let f : int -> string = $(c05 (<< assert false >> : (int, string) eql expr))
