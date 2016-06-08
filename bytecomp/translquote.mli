open Typedtree
open Lambda

val quote_expression : (expression -> lambda) -> expression -> lambda

val transl_close_expression : Location.t -> lambda -> lambda

