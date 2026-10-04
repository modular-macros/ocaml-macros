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

(* Modules in compile-time code are evaluated there. *)

module type S = sig type t val v : t val show : t -> string end

$let literal =
  let module M = struct
    let v = 1
    module N = struct let w = v + 1 end
    exception E of int
    let f () = try raise (E 3) with E n -> n
  end in
  M.v + M.N.w + M.f ()

$let packed = (module struct type t = int let v = 2 let show = string_of_int end : S)
$let unpacked = let module M = (val packed) in M.show M.v
$let[..] shown (module M : S) = Expr.string (M.show M.v)

module Static = struct $let v = 3 let r = 4 end
$let of_static = (module struct type t = int let v = Static.v let show = string_of_int end : S)
$let aliased = let module A = Static in A.v
let aliased_at_run_time = let module A = Static in A.r + $(Expr.int A.v)

(* State in a module of compile-time code persists through compile
   time. *)
module type Counter = sig val count : int ref end
$let counter = (module struct let count = ref 0 end : Counter)
$let next () = let module C = (val counter) in incr C.count; !C.count
$let first = next ()
$let second = next ()

(* A package passed to a dependent parameter *)
module type I = sig type _ t val get : 'a. 'a t -> int end
$let[..] use : (module M : I) -> 'a M.t -> int expr =
  fun (module M : I) x -> Expr.int (M.get x)
module Lengths = struct type 'a t = 'a list  $let get = List.length end

(* A literal structure in a generator may quote the run-time values in
   scope. *)
let scale = 10
$let[..] scaled x = let module M = struct let c = << $x * scale >> end in M.c

let () =
  Printf.printf "%d %s %s %s %d %d %d %d %d %d %d\n"
    $(Expr.int literal) $(Expr.string unpacked) $(shown packed)
    $(shown of_static) $(Expr.int aliased) aliased_at_run_time
    $(Expr.int first) $(Expr.int second)
    $(use (module Lengths : I) [1; 2; 3]) $(use (module Lengths) [1])
    $(scaled << 2 >>)
