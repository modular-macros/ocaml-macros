(* The library side of template_app_mixed_result_nested.ml: a template
   application with a MIXED result inside a plain functor body nested
   under a template functor body (the nested S1 lift,
   SPLITTING-FUNCTORS.md).  The enclosing plain functor is therefore
   mixed, so the chain pass reaches the application through the
   shared-block walk, and the inline-pair branch binds the component
   call once per OUTER instantiation, record and dyn off one pair. *)

module G[Y : sig macro m : unit -> int expr end] = struct
  let gv = 10
  macro gm () = Y.m ()
  macro gq () = << gv + 1 >>    (* quotes G's body binding: env slot *)
end

module V0 = struct macro m () = << 5 >> end
module V1 = struct macro m () = << 3 >> end

module F[X : sig macro m : unit -> int expr end] = struct
  module P (Q : sig val d : int end) = struct
    module App1 = G[V0]
    module App2 = G[V1]
    module _ = G[V0]            (* anonymous mixed-result application *)
    let pv = App1.gv + App2.gv + Q.d
  end
  module Z0 = P (struct let d = 100 end)  (* applied inside the template body *)
  let w = $( Z0.App2.gq () )    (* direct body-splice item off the nested result *)
  macro fm () = << $( Z0.App1.gq () ) + $( X.m () ) >>
end
