(* TEST
 flags = "-dparsetree -stop-after parsing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The parse tree of static bindings and generators, with and without
   an extension node *)

$let a = 1
$let[..] b x = x
$let%ext c = 2
$let[..]%ext d x = x

module type S = sig
  $val a : int
  $val[..] b : 'a -> 'a
  $val%ext c : int
  $val[..]%ext d : 'a -> 'a
end
