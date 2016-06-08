open CamlinternalQuote

external ast_to_expr : Exp.t -> 'a expr = "%identity"

let of_int i =
  ast_to_expr @@ Exp.constant (Loc.none) @@
  Constant.integer i

let of_float f =
  ast_to_expr @@ Exp.constant (Loc.none) @@
  Constant.floating f

let of_string s =
  ast_to_expr @@ Exp.constant (Loc.none) @@
  Constant.string s

