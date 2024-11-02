val quote_expression : Lambda.lambda -> Lambda.lambda

val transl_close_expression : Location.t -> Ident.Set.t -> Lambda.lambda -> Lambda.lambda

val fv : Typedtree.expression -> Ident.Set.t
