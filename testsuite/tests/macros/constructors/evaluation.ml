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

(* Compile-time code raises and matches compile-time constructors. *)

exception $Stop of int
exception $Carry of int expr
exception $Fields of { n : int; s : string }

type t = ..
type t += $A | B | $C of int

$let rec search n = if n * n > 50 then raise (Stop n) else search (n + 1)
$let found = try search 0 with Stop n -> n
$let rank = function A -> 1 | C n -> n | _ -> 0
$let field = try raise (Fields { n = 4; s = "s" }) with Fields r -> r.n
$let name = Printexc.to_string (Stop 3)
$let slot = Obj.Extension_constructor.name [%extension_constructor Stop]

$let[..] carried () = try raise (Carry << 5 >>) with Carry c -> << $c + 1 >>

exception $Halt = Stop
$let rebound = try raise (Halt 6) with Stop n -> n

module M = struct
  exception $Inner of int
  $let inner = try raise (Inner 7) with Inner n -> n
end
module F (X : sig end) = struct
  exception $Fresh
  $let fresh = match raise Fresh with exception Fresh -> 8 | () -> 0
end
module FA = F (struct end)

type _ Effect.t += $Ask : int -> int Effect.t
$let answered =
  Effect.Deep.try_with (fun () -> Effect.perform (Ask 6) + 1) ()
    { effc = fun (type a) (e : a Effect.t) ->
        match e with
        | Ask n ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
                    Effect.Deep.continue k (n * 2))
        | _ -> None }

$let type_of_exception : type a. a -> exn -> int =
  fun _ -> function M.Inner n -> n | Stop n | Halt n -> n | _ -> 9

let () =
  Printf.printf "%d %d %d %d %s %s\n"
    $(Expr.int found) $(Expr.int (rank A)) $(Expr.int (rank (C 3)))
    $(Expr.int field) $(Expr.string name) $(Expr.string slot);
  Printf.printf "%d %d %d %d %d\n"
    $(carried ()) $(Expr.int rebound) $(Expr.int M.inner)
    $(Expr.int FA.fresh) $(Expr.int (type_of_exception () Not_found));
  Printf.printf "%d %d\n" (match B with B -> 10 | _ -> 0)
    $(Expr.int answered);
  print_int $(try raise (Stop 11) with Stop n -> Expr.int n);
  print_newline ()
