val const : 'a -> Lambda.lambda

val quote_expression : Lambda.lambda -> Lambda.lambda

val quote_access : Lambda.lambda -> Lambda.lambda

val transl_close_function :
  Location.t -> Ident.Set.t -> Lambda.lambda ->
  Ident.t list * Ident.t list * Lambda.lambda
val transl_close_quotation : Location.t -> Ident.Set.t -> Lambda.lambda -> Lambda.lambda

val fv : Typedtree.expression -> Ident.Set.t

val remove_events : Lambda.lambda -> Lambda.lambda

val quote_module_lambda : Lambda.lambda -> Lambda.lambda

val module_builder : Lambda.lambda -> Lambda.lambda
