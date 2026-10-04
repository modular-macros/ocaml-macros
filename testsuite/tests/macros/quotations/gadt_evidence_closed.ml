(* TEST
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The code that leaves the arm has no free variable but the evidence
   of the equation that it uses. *)

type (_, _) eql = Refl : ('a, 'a) eql

let s : string =
  $(let stash : type b. (int, b) eql expr -> b expr =
      fun r ->
        let cell = ref None in
        let _ : unit expr =
          << match $r with
             | Refl -> $(cell := Some << (1 : b) >>; << () >>) >> in
        Option.get !cell
    in
    stash (<< assert false >> : (int, string) eql expr))
