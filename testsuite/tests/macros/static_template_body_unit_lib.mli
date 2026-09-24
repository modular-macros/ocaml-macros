module type S = sig $val v : int end

(* A template functor's result may declare the static. *)
module F : [X : S] sig $val s : int  val a : int end
