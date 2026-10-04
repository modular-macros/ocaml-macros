(* Templates whose results have static bindings, at their top level and
   in a module, the second through a coerced alias member. *)
module type Setoid = sig type t $val compare : t -> t -> int end

module Id1 $(X : Setoid) = struct
  type t = X.t
  $let top : int = 3
  module M = struct
    $let k : int = 7
    let v = 5
    $let[..] g c = << $c + v >>
  end
end

module type H = sig
  module F : $(X : Setoid) -> sig
    module M : sig $val k : int  val v : int  $val[..] g : int expr -> int expr end
  end
end
module K = struct
  $let other : int = 8
  $let k : int = 7
  let w = 100
  let v = 5
  $let[..] g c = << $c + v + w >>
end
module Id0 $(X : Setoid) = struct module M = K end
module HI : H = struct module F = Id0 end

module IntS = struct
  type t = int
  $let compare : t -> t -> int = fun a b -> compare a b
end
