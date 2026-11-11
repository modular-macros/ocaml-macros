type call = { start : string; duration : string; pid : string; program : string; dir : string;
              exit : string; args : string list; outputs : string list;
              rusage : (string * string) list }

val read : string -> call list

val record : Table.t -> call -> unit
