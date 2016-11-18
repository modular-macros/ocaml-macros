macro of_bool : bool -> bool expr
macro of_int : int -> int expr
macro of_float : float -> float expr
macro of_char : char -> char expr
macro of_string : string -> string expr
macro of_list : ('a -> 'a expr) -> 'a list -> 'a list expr

