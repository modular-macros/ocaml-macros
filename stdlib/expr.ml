open ^CamlinternalQuote
open ^Pervasives

static of_int i =
  ast_to_expr @@ Exp.constant (Loc.none) @@
  Constant.integer i

static of_float f =
  ast_to_expr @@ Exp.constant (Loc.none) @@
  Constant.floating f

static of_string s =
  ast_to_expr @@ Exp.constant (Loc.none) @@
  Constant.string s

