(* The library side of mixed_result_nested.ml: a template application
   with a mixed result inside a functor body nested under a template
   body.  The enclosing functor is therefore mixed, so the compile-time
   pass reaches the application through the shared block, binding the
   component call once per outer instantiation. *)

module G$(Y : sig $val[..] m : unit -> int expr end) = struct
  let gv = 10
  $let[..] gm () = Y.m ()
  $let[..] gq () = << gv + 1 >>    (* quotes G's body binding: env slot *)
end

module V0 = struct $let[..] m () = << 5 >> end
module V1 = struct $let[..] m () = << 3 >> end

module F$(X : sig $val[..] m : unit -> int expr end) = struct
  module P (Q : sig val d : int end) = struct
    module App1 = G$(V0)
    module App2 = G$(V1)
    module _ = G$(V0)            (* anonymous mixed-result application *)
    let pv = App1.gv + App2.gv + Q.d
  end
  module Z0 = P (struct let d = 100 end)  (* applied inside the template body *)
  let w = $( Z0.App2.gq () )    (* direct body-splice item off the nested result *)
  $let[..] fm () = << $( Z0.App1.gq () ) + $( X.m () ) >>
end
