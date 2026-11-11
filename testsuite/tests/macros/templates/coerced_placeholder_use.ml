module M = struct
  module D = struct let x = 1 let y = 2 end
  module G = Coerced_placeholder_lib.T$(D)
end
module A = struct
  module N = struct
    module D = struct let x = 3 let y = 4 end
    module G = Coerced_placeholder_lib.T$(D)
  end
end
