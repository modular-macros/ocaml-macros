(* TEST
 toplevel;
*)

(* Scope extrusion in toplevel phrases: a variable escaping its
   quotation's scope surfaces as a clean per-phrase error -- the FreeVar
   condition of the quotation check, mapped in topeval -- never as an
   "unbound at toplevel" fatal.  The accomplice shapes match the batch
   scope_extrusion_* tests. *)

(* Through a ref cell. *)
macro m_state () =
  let r = ref << 0 >> in
  let _ = << fun x -> $(r := <<x>> ; <<x>>) >> in
  !r
;;

let y = $(m_state ())
;;

(* Through an effect handler that discards the continuation. *)
type _ Effect.t += R : int expr -> int expr Effect.t
;;

macro m_effect () =
  match << fun x -> $(Effect.perform (R <<x>>)) >> with
  | y -> y
  | effect R z, _k -> << fun _ -> $z + 1 >>
;;

let w = $(m_effect ()) 10
;;

(* Through an exception's payload. *)
exception E of int expr
;;

macro m_exn () =
  try
    let _ = << fun x -> $(raise (E <<x>>)) >> in << 0 >>
  with E x -> x
;;

let z = $(m_exn ())
;;

(* Resuming a one-shot quote-building continuation twice is reported as
   the raised exception, not as an extrusion -- and fails only the
   phrase. *)
macro m_multi () =
  match << fun x -> $(Effect.perform (R <<x>>)) >> with
  | y -> y
  | effect R z, k ->
      Effect.Deep.continue k << $z + 1 >> ;
      Effect.Deep.continue k << $z + 2 >>
;;

let v = $(m_multi ()) 10
;;

(* The legal twins: a nested quotation of an enclosing quotation's binder
   is not a run-time name -- it resolves against the enclosing build's
   binder, as in batch compilation. *)
macro m_ok () = << fun x -> $( << x >> ) >>
;;

let ok = $(m_ok ()) 42
;;

let helper n = n + 1
;;

macro m_mixed () = << fun x -> $( << helper x >> ) >>
;;

let ok2 = $(m_mixed ()) 41
;;

(* The session survives all of the failures above. *)
let alive = ok + ok2
;;
