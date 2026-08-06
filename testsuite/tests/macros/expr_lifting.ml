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

(* One test per function of the stdlib Expr module: each value is
   lifted into code at compile time, in a splice, and the spliced
   program checks it arrives intact.  [Expr] is defined in terms of
   lambda, so back-end divergence is unlikely; the native run is a
   parity check rather than a second sweep. *)

(* The type the interface exports for a lifted value. *)
let () =
  Printf.printf "t: %d\n" $(let e : int Expr.t = Expr.int 8 in e)

let () = Printf.printf "unit: %b\n" ($(Expr.unit ()) = ())

let () = Printf.printf "bool: %b %b\n" $(Expr.bool true) $(Expr.bool false)

let () = Printf.printf "char: %c\n" $(Expr.char 'q')

let () =
  Printf.printf "uchar: %X\n" (Uchar.to_int $(Expr.uchar (Uchar.of_int 0x1F42B)))

let () = Printf.printf "int: %d %d\n" $(Expr.int 42) $(Expr.int (-7))

let () = Printf.printf "int32: %ld\n" $(Expr.int32 (-3l))

let () = Printf.printf "int64: %Ld\n" $(Expr.int64 9_000_000_000L)

let () = Printf.printf "nativeint: %nd\n" $(Expr.nativeint 5n)

let () =
  Printf.printf "float: %g %g %b %b\n"
    $(Expr.float 3.5) $(Expr.float (-0.125))
    (Float.is_nan $(Expr.float Float.nan))
    ($(Expr.float Float.infinity) = infinity)

let () = Printf.printf "string: %s\n" $(Expr.string "lifted text")

let () =
  let z = $(Expr.complex { Complex.re = 1.5; im = -0.5 }) in
  Printf.printf "complex: %g %g\n" z.Complex.re z.Complex.im

let () =
  Printf.printf "option: %d %b\n"
    (match $(Expr.option (Some (Expr.int 9))) with Some n -> n | None -> 0)
    ($(Expr.option (None : int expr option)) = None)

let () =
  Printf.printf "list: %d %b\n"
    (List.fold_left (+) 0 $(Expr.list [Expr.int 1; Expr.int 2; Expr.int 3]))
    ($(Expr.list ([] : int expr list)) = [])

let () =
  Printf.printf "either: %d %s\n"
    (match $(Expr.either (Either.Left (Expr.int 4)
                          : (int expr, string expr) Either.t)) with
     | Either.Left n -> n | Either.Right _ -> 0)
    (match $(Expr.either (Either.Right (Expr.string "right")
                          : (int expr, string expr) Either.t)) with
     | Either.Left _ -> "" | Either.Right s -> s)

let () =
  Printf.printf "result: %d %s\n"
    (match $(Expr.result (Ok (Expr.int 6)
                          : (int expr, string expr) Result.t)) with
     | Ok n -> n | Error _ -> 0)
    (match $(Expr.result (Error (Expr.string "boom")
                          : (int expr, string expr) Result.t)) with
     | Ok _ -> "" | Error s -> s)

let () =
  let (a, b) = $(Expr.pair (Expr.int 1, Expr.string "two")) in
  Printf.printf "pair: %d %s\n" a b

let () =
  let a = $(Expr.iarray 3 (fun i -> Expr.int (10 * i))) in
  Printf.printf "iarray: %d %d %d %d %d\n"
    (Iarray.length a) (Iarray.get a 0) (Iarray.get a 1) (Iarray.get a 2)
    (Iarray.length $(Expr.iarray 0 (fun _ -> Expr.int 0)))

let () =
  let fa = $(Expr.iarray 2 (fun i -> Expr.float (0.5 +. float_of_int i))) in
  Printf.printf "iarray float: %g %g %b\n"
    (Iarray.get fa 0) (Iarray.get fa 1)
    (Obj.tag (Obj.repr fa) = Obj.double_array_tag)

let () =
  (* Elements with a free variable: the array's free-variable set is
     the union of its elements'. *)
  let spread =
    $(Expr.func (fun x ->
        Expr.iarray 2 (fun i -> if i = 0 then x else << $x * 2 >>))) in
  let a = spread 21 in
  Printf.printf "iarray fv: %d %d\n" (Iarray.get a 0) (Iarray.get a 1)

let () =
  Printf.printf "iarray reject: %s\n"
    $(match Expr.iarray (-1) (fun _ -> Expr.int 0) with
      | _ -> Expr.string "accepted"
      | exception Invalid_argument _ -> Expr.string "rejected")

let () =
  let inc = $(Expr.func (fun x -> << $x + 1 >>)) in
  Printf.printf "func: %d\n" (inc 41)

let () =
  let (a, b, c) = $(Expr.unsafe_lift_constant (1, "two", 3.5)) in
  Printf.printf "unsafe_lift_constant: %d %s %g\n" a b c

let () =
  let l = $(Expr.unsafe_lift_constant [ (1, 2l); (3, 4l) ]) in
  let z = $(Expr.unsafe_lift_constant { Complex.re = 1.5; im = -0.5 }) in
  Printf.printf "unsafe_lift_constant blocks: %b %g %g\n"
    (l = [ (1, 2l); (3, 4l) ]) z.Complex.re z.Complex.im

let () =
  let fa = $(Expr.unsafe_lift_constant [| 1.5; 2.5 |]) in
  let fla = $(Expr.unsafe_lift_constant (Float.Array.of_list [ 0.25 ])) in
  Printf.printf "unsafe_lift_constant floats: %g %g %b %g\n"
    fa.(0) fa.(1) (Obj.tag (Obj.repr fa) = Obj.double_array_tag)
    (Float.Array.get fla 0)

let () =
  Printf.printf "unsafe_lift_constant bytes: %s\n"
    (Bytes.to_string $(Expr.unsafe_lift_constant (Bytes.of_string "seq")))

let () =
  Printf.printf "unsafe_lift_constant reject: %s\n"
    $(match Expr.unsafe_lift_constant (fun (x : int) -> x) with
      | _ -> Expr.string "accepted"
      | exception Invalid_argument _ -> Expr.string "rejected")
