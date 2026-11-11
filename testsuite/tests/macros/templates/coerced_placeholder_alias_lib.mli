module E $(A : sig end) : sig
  module Mark : sig val get : int -> int val set : int -> unit end
end
module T : sig module Mark : sig val get : int -> int end end
