(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* A splice outside a quotation is evaluated when its unit is compiled,
   and the code that it builds takes its place. *)

let a = $(<< 1 + 2 >>)
let b = Printf.sprintf "%d %S %C" $(<< 1 >>) $(<< "2" >>) $(<< '3' >>)
let c = $(<< (4l, 5L, 6n, 7.5) >>)
let d = 8l

(* Compile-time code may use the standard library, and what it prints
   is printed by the compiler. *)

let e =
  $(let n = List.length [1; 2; 3] in
    print_endline "compiling";
    if n > 2 then << "long" >> else << "short" >>)

(* The code that a splice builds may mention the run-time variables in
   whose scope the splice stands. *)

let f = 10
let g = $(<< f + 1 >>)
let cube x =
  $(let rec pow n c =
      if n = 0 then << 1 >> else << $c * $(pow (n - 1) c) >> in
    pow 3 << x >>)
let h = function (1, x) | (x, _) -> $(<< x + f >>)
let rec fact n = if n = 0 then 1 else n * $(<< fact (n - 1) >>)

(* A binder of the code that a splice builds does not capture a
   variable of its context, nor is it captured by one. *)

let i x = $(let wrap c = << let x = 42 in $c + x >> in wrap << x >>)

(* A splice is evaluated once, wherever it stands. *)

let j () = for _ = 1 to 3 do $(print_endline "once"; << () >>) done

let () =
  let (c1, c2, c3, c4) = c in
  Printf.printf "%d %s %ld %Ld %nd %g %ld %s %d %d %d %d %d\n"
    a b c1 c2 c3 c4 d e g (cube 3) (h (1, 5)) (fact 5) (i 7);
  j ()
