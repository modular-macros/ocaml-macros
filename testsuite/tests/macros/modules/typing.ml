(* TEST
 flags = "-i -stop-after typing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Compile-time code may bind modules: a literal structure, an unpacked
   package, an alias of a module whose components it may use.  A
   package type is read at the level of the package: the members of a
   package built in compile-time code are compile-time values, as the
   static bindings of a module are. *)

module type S = sig type t val v : t val show : t -> string end

$let literal =
  let module M = struct
    let v = 1
    module N = struct let w = v + 1 end
  end in
  M.v + M.N.w

$let packed = (module struct type t = int let v = 2 let show = string_of_int end : S)
$let unpacked = let module M = (val packed) in M.show M.v
$let[..] shown (module M : S) = Expr.string (M.show M.v)
$let[..] rec shown_all : (module S) list -> string expr = function
  | [] -> << "" >>
  | m :: ms -> << $(shown m) ^ $(shown_all ms) >>

module Static = struct $let v = 3 let r = 4 end
$let of_static = (module struct type t = int let v = Static.v let show = string_of_int end : S)
$let aliased = let module A = Static in A.v
$let opened = let open Static in v
let aliased_at_run_time = let module A = Static in A.r + $(Expr.int A.v)

$let packs : (module S) list = [packed; of_static]

(* A package of types alone may name any module. *)
module type Types = sig type t end
module R = struct type t = int let v = 1 end
$let inert = (module R : Types)
module type Counter = sig val count : int ref end
$let mutable_state = (module struct let count = ref 0 end : Counter)
