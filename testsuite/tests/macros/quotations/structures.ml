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

(* A quotation may contain a structure: the code that it builds
   defines a module, as a package that a splice delivers and a module
   binding unpacks, or bound and used within the quotation. *)

module type S = sig
  type t
  val v : t
  val show : t -> string
  exception E of t
end

$let[..] make (n : int expr) : (module S) expr =
  << (module struct
        type t = int
        let hidden = 2
        let v = $n * hidden
        let show = string_of_int
        exception E of t
      end : S) >>

(* A module bound to a spliced package. *)
module X = (val $(make << 21 >>))
let () =
  print_endline (X.show X.v);
  (try raise (X.E X.v) with X.E n -> print_endline (X.show n))

(* Members made from compile-time data, using each other. *)
module type Fields = sig
  val names : string list
  val get : int -> string
  val len : 'a list -> int
end
$let fields : string list = ["a"; "b"; "c"]
$let[..] make_fields () : (module Fields) expr =
  let lits =
    List.fold_right (fun s acc -> << $(Expr.string s) :: $acc >>)
      fields << [] >>
  in
  << (module struct
        let names = $lits
        let (first, _) = (List.hd names, ())
        external id : 'a -> 'a = "%identity"
        let get i = id (List.nth names i)
        let rec len = function [] -> 0 | _ :: l -> 1 + len l
      end : Fields) >>
module Y = (val $(make_fields ()))
let () =
  Printf.printf "%s %s %d\n" (String.concat "," Y.names) (Y.get 1)
    (Y.len Y.names)

(* Each splice builds its own module: the types are distinct and so are
   the values. *)
$let[..] both () =
  << let module A = (val $(make << 1 >>)) in
     let module B = (val $(make << 2 >>)) in
     A.show A.v ^ " " ^ B.show B.v >>
let () = print_endline $(both ())

(* Bound in the quotation, with a run-time value captured, nested
   modules and a functor. *)
let base = 10
$let[..] local x =
  << let module M = struct
       let y = $x + base
       module N = struct let z = y * 2 end
       module F (A : sig val z : int end) = struct let u = A.z + 1 end
       module G = F (N)
     end in
     M.G.u >>
let () = Printf.printf "%d\n" $(local << 1 >>)

(* The names an open or an include binds. *)
$let[..] opened () =
  << let open struct let c = 3  exception E of int end in
     try raise (E c) with E n -> n >>
$let[..] included () =
  << let module M = struct
       include struct let a = 1  exception F end
       let b = a + 1
       exception G = F
     end in
     try raise M.F with M.G -> M.b >>
let () = Printf.printf "%d %d\n" $(opened ()) $(included ())

(* In compile-time code the names an open or an include binds are of
   that code's level. *)
$let o : int = let open struct let c = 3 end in c
$let i : int =
  let module M = struct include struct let a = 1 end  let b = a + 1 end in
  M.b
let () = Printf.printf "%d %d\n" $(Expr.int o) $(Expr.int i)
