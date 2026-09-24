(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* C12: the equation escapes with no free variable at all -- the
   stashed code is closed -- so only the evidence catches it. *)

type (_, _) eql = Refl : ('a, 'a) eql

$gen c12 : type b. (int, b) eql expr -> b expr =
  fun r ->
    let cell = ref None in
    let _ : unit expr =
      << match $r with Refl -> $(cell := Some << (1 : b) >>; << () >>) >> in
    Option.get !cell

let s : string = $(c12 (<< assert false >> : (int, string) eql expr))
