open Typedtree
open Lambda

val quote_expression : (expression -> lambda) -> expression -> lambda

val transl_close_expression : Location.t -> lambda -> lambda

(** Whether calls should be made to [CamlinternalQuote] or to its lifted
    version. Static code should call the lifted version and not runtime code.
    False by default. *)
val lifted_path : bool ref

