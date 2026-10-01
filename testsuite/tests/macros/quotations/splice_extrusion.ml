(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Code that mentions a variable of a quotation outside its scope is
   reported at the splice whose result it is. *)

let a =
  $(let r = ref << 0 >> in
    let _ = << fun x -> $(r := << x >>; << x >>) >> in
    !r)
