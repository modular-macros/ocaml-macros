(* TEST
 toplevel;
*)

(* Square brackets are not operator characters, so the lexer never
   fuses two adjacent template brackets into one token: "[]", "]]",
   "][" and "[[" all lex as intended, with no spacing discipline.

   The applications below use a PLAIN functor F, so the phrases that
   parse are rejected by the type checker with a kind mismatch: the
   typing error is what shows that parsing succeeded.

   The one exception is ">]", which the lexer reads as a single
   token: a module type ending in an object type needs a space before
   the closing bracket.  See parsing/CONFLICTS.md. *)

module type S = sig end
module type A = sig end
module type B = sig end
module type C = sig end
module type OBJ = sig type t end
module F(X : S) = struct end
;;

(* Empty brackets: "[]" is two tokens. *)
module G[] = struct end
;;

(* "]]": a nested application closes with no space. *)
module M = F[F[struct end]]
;;

(* "][": adjacent argument lists. *)
module M = F[struct end][struct end]
;;

(* "[[": nested template types open with no space. *)
module type T = [[A] B] C
;;

(* The ">]" exception: an object-type tail fuses with the closing
   bracket ... *)
module type T = [X : OBJ with type t = < m : int >] B
;;

(* ... and needs a space. *)
module type T = [X : OBJ with type t = < m : int > ] B
;;
