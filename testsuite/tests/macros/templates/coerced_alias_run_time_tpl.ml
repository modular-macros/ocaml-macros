module T $(X : sig end) : sig module H : sig val x : int end end = struct
  module H = Coerced_alias_run_time_lib.B
end
module M = T$(struct end)
