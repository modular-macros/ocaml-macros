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

(* A member of a template's result that holds only aliases, [T] of
   [Coprod] here, has no compile-time content of its own and is a dummy
   in the record of an application.  Its coerced view is built from the
   modules aliased.  The coercion of the record gave the dummy instead,
   and reading a static binding through the view killed the static
   program with a segmentation fault. *)

module type TYPE = sig type t end
module type OPS = sig
  type t
  $val zero : t
  $val add : t -> t -> t
end
module type Algebra = sig
  module T : TYPE
  module Op : OPS with type t := T.t
end

module type COPRODUCT = sig
  module T : Algebra
  module A : Algebra
  $val inl : A.T.t -> T.T.t
  $val out : T.T.t -> int
end

module Num $(X : TYPE) = struct
  type t = int
  $let zero : t = 0
  $let add : t -> t -> t = fun x y -> x + y
end

module Split $(M : OPS) = struct module T = M module Op = M end

module Coprod $(A : Algebra) $(X : TYPE) = struct
  module MA = Num$(X)
  module T = struct
    module T = MA
    module Op = MA
  end
  module A = A
  $let inl : A.T.t -> T.T.t = fun _ -> 1
  $let out : T.T.t -> int = fun n -> n
end

module Ext $(A : Algebra)
    $(C : $(X : TYPE) -> COPRODUCT with module A.T = A.T) =
struct
  module PS $(X : TYPE) = struct
    module CX = C$(X)
    module Alg = CX.T
    $let sta : A.T.t -> Alg.T.t = fun x -> CX.inl x
    $let sum : Alg.T.t list -> int = fun xs ->
      CX.out (List.fold_left Alg.Op.add Alg.Op.zero xs)
  end
end
module Int = struct
  type t = int
  $let zero : t = 0
  $let add : t -> t -> t = fun x y -> x + y
end
module Make $(A : OPS with type t = int) = struct
  module SA = Split$(A)
  module CP_A = Coprod$(SA)
  module CX $(X : TYPE) = struct
    include CP_A$(X)
  end
  module FE0 = Ext$(SA)
  module FE = FE0$(CX)
end
module Unit = struct type t = unit end
module M = Make$(Int)
module P = M.FE.PS$(Unit)
$let total : int = P.sum [P.sta 5; P.sta 6; P.sta 7]
let () = Printf.printf "%d\n" $(Expr.int total)
