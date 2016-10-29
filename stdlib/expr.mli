static val of_bool : bool -> bool expr
static val of_int : int -> int expr
static val of_float : float -> float expr
static val of_char : char -> char expr
static val of_string : string -> string expr
static val of_list : ('a -> 'a expr) -> 'a list -> 'a list expr

