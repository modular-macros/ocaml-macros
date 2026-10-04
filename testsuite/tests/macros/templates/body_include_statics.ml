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

(* A template body that includes an application of another template:
   the static bindings the include brings in are compile-time members
   of the including body.  They used to be taken for run-time members,
   a static binding recording the level of a run-time value, and were
   given dummy slots: calling one killed the static program with a
   segmentation fault. *)

module type S = sig type t $val compare : t -> t -> int end

module Mset $(E : S) : sig
  type t
  $val empty : t
  $val singleton : E.t -> t
  $val mem : E.t -> t -> bool
  $val[..] size : t expr -> int expr
  val length : t -> int
  val nil : t
end = struct
  type t = E.t list
  $let empty : t = []
  $let singleton : E.t -> t = fun x -> [ x ]
  $let mem : E.t -> t -> bool =
    fun x s -> List.exists (fun y -> E.compare x y = 0) s
  let length = List.length
  let nil = []
  $let[..] size s = << length $s >>
end

(* The include directly in the body, and in a module of the body
   beside a static binding of its own. *)
module Parse $(Term : S) = struct
  include Mset$(Term)
  $let one : Term.t -> t = fun c -> singleton c
  module C = struct
    include Mset$(Term)
    $let both : Term.t -> Term.t -> bool = fun c d -> mem c (singleton d)
  end
  let three () : int = $(size << nil >>) + 3
end

module T = struct
  type t = int
  $let compare : t -> t -> int = fun x y -> Stdlib.compare x y
end

module P = Parse$(T)

let () =
  Printf.printf "%b %b %b %d\n"
    $(Expr.bool (P.mem 1 (P.one 1)))
    $(Expr.bool (P.C.both 1 2))
    $(Expr.bool (P.C.mem 2 P.C.empty))
    (P.three ())
