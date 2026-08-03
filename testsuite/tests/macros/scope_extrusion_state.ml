(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Extrusion with references: [<<x>>] is smuggled out of its quotation
   through a ref cell; the splice of the smuggled code fails the
   quotation check, reported as a located error. *)
macro m () =
  let r = ref << 0 >> in
  let _ = << fun x -> $(r := <<x>> ; <<x>>) >> in
  !r

let y = $(m ())
