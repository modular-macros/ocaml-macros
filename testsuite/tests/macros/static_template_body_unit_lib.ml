module type S = sig $val v : int end

module F [X : S] = struct
  $let s = X.v + 1
  let a = $(Expr.int s)
end
