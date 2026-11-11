type t = { path : string; modname : string; bytes : int }

val read : string -> t

val record : Table.t -> t -> unit
