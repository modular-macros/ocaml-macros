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

(* Templates are applicative: two applications to one path share their
   types, so the values of one are the values of the other, while each
   application still runs the body's compile-time code and has its own
   state. *)

module type S = sig type u val show : u -> string end

module F $(X : S) : sig
  type t
  type d = D of X.u
  val mk : X.u -> t
  val get : t -> X.u
  val count : unit -> int
  $val[..] twice : t expr -> (X.u * X.u) expr
  $val applications : int
end = struct
  type t = X.u
  type d = D of X.u
  let mk x = x
  let get x = x
  let counter = ref 0
  let count () = incr counter; !counter
  $let[..] twice c = << (get $c, get $c) >>
  $let applications = (prerr_endline "applied"; 1)
end

module V = struct type u = int let show = string_of_int end
module A = F$(V)
module B = F$(V)

let x : A.t = B.mk 3
let y : F$(V).t = A.mk 4
let (A.D d) = B.D 5
let (a, _) = $(A.twice << x >>)
let (b, _) = $(B.twice << y >>)

(* Parameters of both kinds. *)
module G $(X : S) (Y : S) : sig type t val mk : X.u -> Y.u -> t val show : t -> string end =
struct
  type t = X.u * Y.u
  let mk x y = (x, y)
  let show (x, y) = X.show x ^ "," ^ Y.show y
end
module W = struct type u = bool let show = string_of_bool end
module GV = G$(V)
module C = GV(W)
module E = GV(W)
let z : G$(V)(W).t = C.mk 1 true

let () =
  ignore (A.count ());
  Printf.printf "%d %d %d %d %d %s\n" a b d (A.count ()) (B.count ()) (E.show z)
