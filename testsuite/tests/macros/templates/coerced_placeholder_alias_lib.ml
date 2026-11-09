module E $(A : sig end) = struct
  module Mark = struct let get _ = 1 let set _ = () end
end
module T = struct
  module G0 = E$(struct end)
  module Mark = G0.Mark
end
