(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* The builders of Expr for dispatch and recursion, in top-level
   splices: switches on immediate values and on strings, and groups of
   recursive functions. *)

type colour = Red | Green | Blue

let vowel : char -> int =
  $(Expr.func (fun c ->
      Expr.switch (module Char) c ~numconsts:256
        [ ('a', << 1 >>); ('e', << 2 >>); ('i', << 3 >>);
          ('o', << 4 >>); ('u', << 5 >>) ]
        (Some << 0 >>)))

let name : colour -> string =
  $(Expr.func (fun c ->
      Expr.switch (module struct type t = colour end) c ~numconsts:3
        [ (Red, << "red" >>); (Green, << "green" >>); (Blue, << "blue" >>) ]
        None))

(* An integer outside the range of the table takes the default, or
   raises if there is none. *)
let small : int -> string =
  $(Expr.func (fun n ->
      Expr.switch (module Int) n ~numconsts:3
        [ (0, << "zero" >>); (2, << "two" >>) ] (Some << "other" >>)))

let strict : int -> string =
  $(Expr.func (fun n ->
      Expr.switch (module Int) n ~numconsts:2
        [ (0, << "zero" >>); (1, << "one" >>) ] None))

let kind : char -> string =
  $(Expr.func (fun c ->
      Expr.switch_intervals (module Char) c ~numconsts:256
        [ ([ ('a', 'z'); ('A', 'Z') ], << "letter" >>);
          ([ ('0', '9') ], << "digit" >>) ]
        (Some << "other" >>)))

let half : int -> string =
  $(Expr.func (fun n ->
      Expr.switch_intervals (module Int) n ~numconsts:10
        [ ([ (0, 4) ], << "low" >>); ([ (5, 9) ], << "high" >>) ] None))

let number : string -> int =
  $(Expr.func (fun s ->
      Expr.string_switch s [ ("one", << 1 >>); ("two", << 2 >>) ] << 0 >>))

let fact : int -> int =
  $(Expr.Letrec.group (fun g ->
      Expr.Letrec.bind g (fun fact ->
        << fun n -> if n = 0 then 1 else n * $fact (n - 1) >>)))

(* Functions bound as the code is built: each of the [n] functions of
   the cycle calls the next, and is bound when it is first needed. *)
let cycle : int -> int =
  $(Expr.Letrec.group (fun g ->
      let n = 3 in
      let table = Array.make n None in
      let rec f i =
        match table.(i) with
        | Some f -> f
        | None ->
            Expr.Letrec.bind g (fun self ->
              table.(i) <- Some self;
              << fun k ->
                   if k = 0 then $(Expr.int i)
                   else $(f ((i + 1) mod n)) (k - 1) >>)
      in
      f 0))

let nested : int -> int =
  $(Expr.Letrec.group (fun outer ->
      let double = Expr.Letrec.bind outer (fun _ -> << fun x -> x * 2 >>) in
      Expr.Letrec.group (fun inner ->
        Expr.Letrec.bind inner (fun count ->
          << fun n -> if n = 0 then 0 else $double 1 + $count (n - 1) >>))))

(* A function bound in the outer group while the inner one is built *)
let across : int -> int =
  $(Expr.Letrec.group (fun outer ->
      Expr.Letrec.group (fun inner ->
        let f =
          Expr.Letrec.bind outer (fun f ->
            << fun n -> if n = 0 then 10 else $f (n - 1) >>)
        in
        Expr.Letrec.bind inner (fun _ -> << fun n -> $f n + 1 >>))))

let raises f x =
  match f x with
  | s -> s
  | exception Invalid_argument m -> "Invalid_argument " ^ m

let () =
  print_endline
    (String.concat " " (List.map (fun c -> string_of_int (vowel c))
                          [ 'a'; 'b'; 'u'; 'z' ]));
  print_endline (String.concat " " (List.map name [ Red; Green; Blue ]));
  print_endline
    (String.concat ", " (List.map small [ 0; 1; 2; 3; -1; max_int ]));
  print_endline
    (String.concat ", " (List.map (raises strict) [ 0; 1; 2; -1 ]));
  print_endline (String.concat " " (List.map kind [ 'q'; 'Q'; '7'; ' ' ]));
  print_endline
    (String.concat ", " (List.map (raises half) [ 0; 7; 10; -3 ]));
  print_endline
    (String.concat " " (List.map (fun s -> string_of_int (number s))
                          [ "one"; "two"; "three" ]));
  Printf.printf "%d %d %d %d %d %d\n"
    (fact 5) (cycle 0) (cycle 1) (cycle 4) (nested 3) (across 2)
