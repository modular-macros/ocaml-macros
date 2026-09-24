(* TEST
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* The programs D-id accepts and S refused, run: the equation is used
   by code built inside a splice, and that code lands in the arm.  The
   evidence costs the generated program nothing: its binder is an
   unused alias and its uses are reads of it, both removed by Simplif. *)

type (_, _) eql = Refl : ('a, 'a) eql

(* C17 *)
$gen c17 : type a b. (a, b) eql expr -> (b expr -> b expr) -> (a -> b) expr =
  fun r f -> << fun x -> match $r with Refl -> $(f << (x : b) >>) >>

let g : int -> int = $(c17 << Refl >> (fun c -> << $c + 1 >>))

(* C19: typecase, consumer form. *)
type _ ty = Int : int ty | Str : string ty | Bool : bool ty

$gen typecase : type a r. a ty expr -> (int expr -> r expr) -> (string expr -> r expr)
                          -> (bool expr -> r expr) -> a expr -> r expr =
  fun r fi fs fb x ->
    << match $r with
       | Int -> $(fi << ($x : int) >>) | Str -> $(fs << ($x : string) >>)
       | Bool -> $(fb << ($x : bool) >>) >>

$gen show : type a. unit -> (a ty -> a -> string) expr = fun () ->
  << fun r x -> $(typecase << r >> (fun c -> << string_of_int $c >>) (fun c -> c)
                           (fun c -> << string_of_bool $c >>) << x >>) >>

$gen size : type a. unit -> (a ty -> a -> int) expr = fun () ->
  << fun r x -> $(typecase << r >> (fun _ -> << 8 >>) (fun c -> << String.length $c >>)
                           (fun _ -> << 1 >>) << x >>) >>

let show_int : int ty -> int -> string = $(show ())
let show_bool : bool ty -> bool -> string = $(show ())
let size_str : string ty -> string -> int = $(size ())

(* C20: typecase, producer form. *)
$gen typecase_gen : type a r. a ty expr -> (r expr -> int expr) -> (r expr -> string expr)
                              -> (r expr -> bool expr) -> r expr -> a expr =
  fun r fi fs fb s ->
    << match $r with Int -> $(fi s) | Str -> $(fs s) | Bool -> $(fb s) >>

$gen read : type a. unit -> (a ty -> string -> a) expr = fun () ->
  << fun r s -> $(typecase_gen << r >> (fun s -> << int_of_string $s >>) (fun s -> s)
                               (fun s -> << bool_of_string $s >>) << s >>) >>

let read_int : int ty -> string -> int = $(read ())

let () =
  Printf.printf "c17: %d\n" (g 41);
  Printf.printf "c19: %s %s %d\n" (show_int Int 42) (show_bool Bool true) (size_str Str "abc");
  Printf.printf "c20: %d\n" (read_int Int "42" + 1)
