external succ : int -> int = "coerced_primitive_succ"
module T $(X : sig end) = struct end
module U = T$(struct end)
