(* TEST
 expect;
*)

(* The code that the builders of Expr make.  A switch on an immediate
   value tests its range before the jump table; the default, or the
   raise that stands for it, is reached from both by one static exit.
   A group of recursive functions is a let rec around its expression. *)

let small = Expr.switch (module Int) << 7 >> ~numconsts:2 [ (0, << "zero" >>) ]
    (Some << "other" >>)
let complete = Expr.switch (module Bool) << true >> ~numconsts:2
    [ (false, << 0 >>); (true, << 1 >>) ] None
;;
[%%expect{|
val small : string expr =
  <code
    (catch
      (let (i = 7)
        (if (&& (>= i 0) (< i 2))
          (switch i case int 0: "zero"
                    default: (exit -1))
          (exit -1)))
     with (-1) "other")>
val complete : int expr =
  <code
    (catch
      (let (i = 1)
        (if (&& (>= i 0) (< i 2)) (switch* i case int 0: 0
                                             case int 1: 1)
          (exit -2)))
     with (-2)
      (raise (makeblock 0 (global Invalid_argument!) "Expr.switch")))>
|}]

(* Mutual recursion: the second function is bound while the first is
   defined, and the first is the argument of its own definition. *)
let even =
  Expr.Letrec.group (fun g ->
    Expr.Letrec.bind g (fun even ->
      let odd =
        Expr.Letrec.bind g (fun _ -> << fun n -> n <> 0 && $even (n - 1) >>)
      in
      << fun n -> n = 0 || $odd (n - 1) >>))
;;
[%%expect{|
val even : (int -> bool) expr =
  <code
    (letrec
      (f (function n (|| (== n 0) (apply f_1 (- n 1))))
        f_1 (function n_1 (&& (!= n_1 0) (apply f (- n_1 1)))))
      f)>
|}]
