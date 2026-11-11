type t

val configure : out:string -> (string * string) list -> t

val append : t -> header:string list -> string list -> unit

val of_args : string list -> (string * string) list * string list
