static module S = struct
  let x = 42
end
;;

let x = S.x (* Error: wrong phase *)
;;

static x = S.x
;;

module type St = module type of S

module F (X : St) = struct
  let y = X.x + 1
end
;;

module M = F(S) (* Error: wrong phase *)
;;

static module M = F(S) (* Error wrong phase *)
;;

static module F (X : St) = struct
  let y = ~Pervasives.succ X.x
end
;;

static module M = F(S) (* OK *)
;;

let () = Printf.printf "%d\n" $(Expr.of_int M.y)
;;

static module G (X : module type of Pervasives) = struct
  let x = X.int_of_string "42"
end
;;

(* Lifting of external module *)
static module N = G(~Pervasives);;

let () = Printf.printf "%d\n" $(Expr.of_int N.x)
;;
