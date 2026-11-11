type site = { modname : string; name : string; kind : Asttypes.functor_kind; file : string;
              line : int }

type t = { path : string; modname : string; definitions : site list; applications : site list }

val read : string -> t

val record : prefix:string -> (string * string) list -> t -> unit
