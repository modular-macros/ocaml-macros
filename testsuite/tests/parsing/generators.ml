(* TEST
 flags = "-dsource -stop-after parsing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Generators.  This test only exercises the syntax.  The mark "[..]"
   is a single token, and follows the keyword with or without a
   space. *)

$let[..] a x = << $x + 1 >>
$let [..] b x = << $x + 2 >>
$let[..] rec c n x = if n = 0 then x else d (n - 1) << $x + 1 >>
and d n x = c n x
$let[..][@inline] e x = x

module M = struct
  let f = 4
  $let[..] g () = << f >>
end

module type S = sig
  val f : int
  $val[..] g : unit -> int expr
  $val [..] h : 'a expr -> 'a expr
end
