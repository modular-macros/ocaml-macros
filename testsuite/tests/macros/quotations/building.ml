(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* A quotation is evaluated to a code value.  The splices inside it are
   evaluated when it is, from left to right.  The bytecode test
   also checks that a bytecode compiler and a native-code compiler
   write the same object for this unit. *)

let trace s c = print_endline s; c

let one = << 1 >>
let add a b = << $a + $b >>
let _ = add one (add one one)

let _ = << $(trace "a" one) + $(trace "b" one) >>
let _ = << ($(trace "c" one), $(trace "d" one), $(trace "e" one)) >>
let _ = << [| $(trace "f" one); $(trace "g" one) |] >>
let _ = << if $(trace "h" << true >>) then $(trace "i" one)
           else $(trace "j" one) >>

let twice () = << $(trace "k" one) >>
let _ = twice ()
let _ = twice ()

(* The variables that a quotation binds are available to the splices
   in their scope. *)

let _ = << fun x -> $(add << x >> one) >>
let _ = << fun (x, y) -> $(add << x >> << y >>) >>
let _ = << fun ~x ?(y = 1) () -> $(add << x >> << y >>) >>
let _ = << let x = 1 in $(add << x >> << x >>) >>
let _ = << let x = 1 and y = 2 in $(add << x >> << y >>) >>
let _ = << let rec f n = if n = 0 then 0 else $(<< f (n - 1) >>) in f 3 >>
let _ = << let rec f n = n = 0 || $(<< g (n - 1) >>)
           and g n = n <> 0 && $(<< f (n - 1) >>) in
           f 3 >>
let _ = << for i = 1 to 3 do ignore $(add << i >> one) done >>
let _ = << match Some 1 with Some x -> $(add << x >> one) | None -> 0 >>
let _ = << function [] -> 0 | x :: _ -> $(add << x >> one) >>
let _ = << function (1, x) | (x, _) -> $(add << x >> one) >>
let _ = << function (1, x) | (x, _) -> $(<< x >>) >>
let _ = << function (1, x) | (x, _) -> $(let y = << x >> in add y one) >>
let _ = let x = << 2 >> in << function (1, y) | (y, _) -> y + $x >>
let _ = << fun x -> match x with y when y > 0 -> $(<< y >>) | _ -> x >>
let _ = << fun f -> try f () with e -> $(<< raise e >>) >>
let _ = << fun x -> $(<< fun y -> $(add << x >> << y >>) >>) >>

(* Constants and control structures *)

let _ = << ("a", 'b', 1.5, 2l, 3L, 4n, [| 5.; 6. |], `C, [7; 8]) >>
let _ = << function "a" -> 1 | "b" -> 2 | _ -> 3 >>
let _ = << function `A -> 1 | `B x -> x | `C (x, _) -> x >>
let _ = << let r = ref 0 in while !r < 3 do incr r done; !r >>
let _ = << fun x -> assert (x > 0); lazy (x + 1) >>
let _ = << fun r -> r := !r + 1 >>

let () = print_endline "done"
