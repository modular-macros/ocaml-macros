module H = Coerced_alias_run_time_lib.B
module T $(X : sig end) = struct end
module M = T$(struct end)
