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

(* GADT refinement in quoted code, end to end: the match on the
   witness is in the generated program, the refined code is built by a
   generator and spliced in, and the program runs (GADTS.md ok2, ok8,
   ok10 through generators and top-level splices). *)

type (_,_) eql = Refl : ('a, 'a) eql

(* ok2: the equation used on the result of a splice in the branch. *)
$gen cast : type a b. (a, b) eql expr -> (a expr -> a expr) -> (a -> b) expr =
  fun r f -> << fun x -> match $r with Refl -> $(f << x >>) >>

let g : int -> int = $(cast << Refl >> (fun c -> << $c + 1 >>))

(* ok8: the match in the splice body, at the generator's stage. *)
$gen coerce : type a b. (a, b) eql -> a expr -> b expr =
  fun r c -> << $(match r with Refl -> c) >>

let h : string = $(coerce Refl << "ok8" >>)

(* ok10: an existential from the generated program's own match,
   carried through a splice in the branch. *)
type _ pack = Pack : 'e * ('e -> int) -> unit pack

$gen unpack : unit pack expr -> int expr = fun p ->
  << match $p with Pack (x, f) ->
       $(let c = << x >> and k = << f >> in << $k $c >>) >>

let n : int = $(unpack << Pack (20, fun v -> v + 22) >>)

let () = Printf.printf "%d %s %d\n" (g 41) h n
