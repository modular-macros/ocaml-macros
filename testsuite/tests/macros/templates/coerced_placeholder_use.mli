module M : sig
  module D : sig val y : int val x : int end
  module G : Coerced_placeholder_lib.S
end
module A : sig module N : sig
  module D : sig val y : int end
  module G : Coerced_placeholder_lib.S
end end
