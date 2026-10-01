(* TEST *)

(* The errors of the builders of Expr for dispatch and recursion, made
   at run time.  Each is raised when the code is built. *)

type colour = Red | Green | Blue

let report what f =
  match f () with
  | (_ : int expr) -> Printf.printf "%s: built\n" what
  | exception Invalid_argument m -> Printf.printf "%s: %s\n" what m

let () =
  report "no table" (fun () ->
    Expr.switch (module Int) << 1 >> ~numconsts:0 [] None);
  report "table too large" (fun () ->
    Expr.switch (module Int) << 1 >> ~numconsts:65536 [] None);
  report "intervals' table too large" (fun () ->
    Expr.switch_intervals (module Int) << 1 >> ~numconsts:65536
      [ ([ (0, 1) ], << 1 >>) ] None);
  report "repeated key" (fun () ->
    Expr.switch (module Char) << 'a' >> ~numconsts:256
      [ ('a', << 1 >>); ('a', << 2 >>) ] None);
  report "key out of range" (fun () ->
    Expr.switch (module Int) << 1 >> ~numconsts:2 [ (2, << 1 >>) ] None);
  report "negative key" (fun () ->
    Expr.switch (module Int) << 1 >> ~numconsts:2 [ (-1, << 1 >>) ] None);
  report "incomplete, no default" (fun () ->
    Expr.switch (module struct type t = colour end) << Red >> ~numconsts:3
      [ (Red, << 1 >>) ] None);
  report "empty interval" (fun () ->
    Expr.switch_intervals (module Char) << 'a' >> ~numconsts:256
      [ ([ ('z', 'a') ], << 1 >>) ] None);
  report "overlapping intervals" (fun () ->
    Expr.switch_intervals (module Char) << 'a' >> ~numconsts:256
      [ ([ ('a', 'm') ], << 1 >>); ([ ('k', 'z') ], << 2 >>) ] None);
  report "no intervals" (fun () ->
    Expr.switch_intervals (module Char) << 'a' >> ~numconsts:256
      [ ([], << 1 >>) ] None);
  report "repeated string" (fun () ->
    Expr.string_switch << "a" >> [ ("a", << 1 >>); ("a", << 2 >>) ] << 0 >>);
  report "not a function" (fun () ->
    Expr.Letrec.group (fun g -> Expr.Letrec.bind g (fun _ -> << 1 >>)));
  let finished = ref None in
  ignore (Expr.Letrec.group (fun g -> finished := Some g; << 0 >>));
  report "outside its group" (fun () ->
    match !finished with
    | Some g ->
        ignore (Expr.Letrec.bind g (fun _ -> << fun x -> x >>)); << 0 >>
    | None -> assert false)

(* A function of a group used outside it is a scope extrusion, found
   when the code that mentions it is built, as for a variable of a
   quotation. *)
let () =
  let escaped = ref None in
  ignore
    (Expr.Letrec.group (fun g ->
       escaped := Some (Expr.Letrec.bind g (fun _ -> << fun x -> x + 1 >>));
       << 0 >>));
  match !escaped with
  | Some f ->
      (match << $f 1 >> with
       | _ -> print_endline "escaped: built"
       | exception Effect.Unhandled _ -> print_endline "escaped: extrusion")
  | None -> assert false
