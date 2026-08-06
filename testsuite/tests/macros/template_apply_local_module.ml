(* TEST
 expect;
*)

(* A template application in a locally bound module is refused during
   type checking, in every context.  Inside a template functor's body
   this used to crash the back end -- the application's pair binder,
   an ident named tapppair, escaped into an expression position that
   nothing bound.  At the top level it was a translator error;
   now both are the same typing error. *)

module type S = sig val v : int end
module G [X : S] = struct let g = X.v * 2 end
module V = struct let v = 1 end
;;

[%%expect{|
module type S = sig val v : int end
module G : [X : S] sig val g : int end
module V : sig val v : int end
|}]

let z = let module I = G[V] in I.g
;;

[%%expect{|
Line 1, characters 23-27:
1 | let z = let module I = G[V] in I.g
                           ^^^^
Error: A template functor cannot be applied in a locally bound module:
       the application is evaluated at compile time, and a module bound
       inside an expression is made at run time.
       Apply it at the top level of a structure ("module M = F[V]")
       and refer to that module here.
|}]

(* The shape that crashed: inside a template functor's body. *)

module F [X : S] = struct
  let f = (let module I = G[X] in I.g)
end
;;

[%%expect{|
Line 2, characters 26-30:
2 |   let f = (let module I = G[X] in I.g)
                              ^^^^
Error: A template functor cannot be applied in a locally bound module:
       the application is evaluated at compile time, and a module bound
       inside an expression is made at run time.
       Apply it at the top level of a structure ("module M = F[V]")
       and refer to that module here.
|}]

(* The application need not be the bound expression itself: anywhere
   inside the locally bound module is refused. *)

module F [X : S] = struct
  let f = (let module M = struct module I = G[X] end in M.I.g)
end
;;

[%%expect{|
Line 2, characters 44-48:
2 |   let f = (let module M = struct module I = G[X] end in M.I.g)
                                                ^^^^
Error: A template functor cannot be applied in a locally bound module:
       the application is evaluated at compile time, and a module bound
       inside an expression is made at run time.
       Apply it at the top level of a structure ("module M = F[V]")
       and refer to that module here.
|}]

let w = let open G[V] in g
;;

[%%expect{|
Line 1, characters 17-21:
1 | let w = let open G[V] in g
                     ^^^^
Error: A template functor cannot be applied in a locally bound module:
       the application is evaluated at compile time, and a module bound
       inside an expression is made at run time.
       Apply it at the top level of a structure ("module M = F[V]")
       and refer to that module here.
|}]

(* The supported form: bind the application as a structure item. *)

module F [X : S] = struct
  module I = G[X]
  let f = I.g
end
module M = F[V]
;;

[%%expect{|
module F : [X : S] sig module I : sig val g : int end val f : int end
module M : sig module I : sig val g : int end val f : int end
|}]
