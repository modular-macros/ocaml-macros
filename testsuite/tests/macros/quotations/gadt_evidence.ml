(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* Code that uses an equation of a match in quoted code, built in a
   splice and landing in the arm of the match *)

type (_, _) eql = Refl : ('a, 'a) eql

let cast : int -> int =
  $(let cast :
      type a b. (a, b) eql expr -> (b expr -> b expr) -> (a -> b) expr =
      fun r f -> << fun x -> match $r with Refl -> $(f << (x : b) >>) >>
    in
    cast << Refl >> (fun c -> << $c + 1 >>))

let coerce : string =
  $(let coerce : type a b. (a, b) eql -> a expr -> b expr =
      fun r c -> << $(match r with Refl -> c) >>
    in
    coerce Refl << "coerced" >>)

type _ pack = Pack : 'e * ('e -> int) -> unit pack

let unpack : int =
  $(let unpack : unit pack expr -> int expr = fun p ->
      << match $p with Pack (x, f) ->
           $(let c = << x >> and k = << f >> in << $k $c >>) >>
    in
    unpack << Pack (20, fun v -> v + 22) >>)

type _ ty = Int : int ty | Str : string ty | Bool : bool ty

let (show_int : int ty -> int -> string),
    (show_bool : bool ty -> bool -> string),
    (size_str : string ty -> string -> int),
    (read_int : int ty -> string -> int) =
  $(let typecase :
      type a r. a ty expr -> (int expr -> r expr) -> (string expr -> r expr)
                -> (bool expr -> r expr) -> a expr -> r expr =
      fun r fi fs fb x ->
        << match $r with
           | Int -> $(fi << ($x : int) >>) | Str -> $(fs << ($x : string) >>)
           | Bool -> $(fb << ($x : bool) >>) >>
    in
    let typecase_gen :
      type a r. a ty expr -> (r expr -> int expr) -> (r expr -> string expr)
                -> (r expr -> bool expr) -> r expr -> a expr =
      fun r fi fs fb s ->
        << match $r with Int -> $(fi s) | Str -> $(fs s) | Bool -> $(fb s) >>
    in
    let show : type a. unit -> (a ty -> a -> string) expr = fun () ->
      << fun r x ->
         $(typecase << r >> (fun c -> << string_of_int $c >>) (fun c -> c)
             (fun c -> << string_of_bool $c >>) << x >>) >>
    in
    let size : type a. unit -> (a ty -> a -> int) expr = fun () ->
      << fun r x ->
         $(typecase << r >> (fun _ -> << 8 >>)
             (fun c -> << String.length $c >>) (fun _ -> << 1 >>) << x >>) >>
    in
    let read : type a. unit -> (a ty -> string -> a) expr = fun () ->
      << fun r s ->
         $(typecase_gen << r >> (fun s -> << int_of_string $s >>)
             (fun s -> s) (fun s -> << bool_of_string $s >>) << s >>) >>
    in
    << ($(show ()), $(show ()), $(size ()), $(read ())) >>)

let () =
  Printf.printf "%d %s %d\n" (cast 41) coerce unpack;
  Printf.printf "%s %s %d %d\n"
    (show_int Int 42) (show_bool Bool true) (size_str Str "abc")
    (read_int Int "42" + 1)
