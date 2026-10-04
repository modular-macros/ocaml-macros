(* A template whose body has an alias member, coerced as a member of a
   signature. *)
module type Setoid = sig type t $val compare : t -> t -> int end

module type H = sig
  module F : $(X : Setoid) -> sig
    type t
    $val show : t -> string
    module M : sig
      $val k : int
      val v : int
      $val[..] g : int expr -> int expr
    end
  end
end

module K = struct
  $let other : int = 8
  $let k : int = 7
  let w = 100
  let v = 5
  $let[..] g c = << $c + v + w >>
end

module Id0 $(X : Setoid) = struct
  type t = X.t
  $let show : t -> string = fun _ -> "id"
  module M = K
end

module HI : H = struct module F = Id0 end

module IntS = struct
  type t = int
  $let compare : t -> t -> int = fun a b -> compare a b
end

(* Curried, and with a member that is itself a template with an alias
   member. *)
module K2 = struct let z = 50  let y = 60  $let j : int = 9 end

module type HC = sig
  module F : $(X : Setoid) -> $(Y : Setoid) -> sig
    module M : sig $val k : int  val v : int end
    module G : $(Z : Setoid) -> sig
      module N : sig val y : int  $val j : int end
    end
  end
end

module Id2 $(X : Setoid) $(Y : Setoid) = struct
  module M = K
  module G $(Z : Setoid) = struct module N = K2 end
end

module HC1 : HC = struct module F = Id2 end

(* A coercion of a coerced template. *)
module type HC2 = sig
  module F : $(X : Setoid) -> $(Y : Setoid) -> sig
    module M : sig val v : int end
  end
end
module HC3 : HC2 = HC1
