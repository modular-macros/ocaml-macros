(* TEST
 readonly_files = "static_program_exit_numbers_lib.ml";
 {
   setup-ocamlc.byte-build-env;
   all_modules = "static_program_exit_numbers_lib.ml \
     static_program_exit_numbers.ml";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
   all_modules = "static_program_exit_numbers_lib.ml \
     static_program_exit_numbers.ml";
   ocamlopt.byte;
   run;
   check-program-output;
 }
*)

(* Static-exception numbers across the static-program boundary
   (the native-static-divergence bug; its trail is in git history).

   A static program compiles a term built by the process that spawned it,
   whose [Lstaticcatch] numbers were drawn from that process's counter,
   while its own counter starts at zero.  Unless it adopts the term's
   numbers first -- bytecomp/static_simplif.ml -- everything it draws
   collides with what the term already uses, and the passes that draw
   them all assume a fresh number cannot occur in the code they wrap.

   Under ocamlopt that was fatal rather than merely wrong: Cmmgen gives
   the loop below the number of the match handler nested inside that very
   loop, and Spill's spill_at_exit fixpoint -- one flat table per
   function, keyed by that number -- then oscillates between the two
   handlers for ever.  Compiling this file natively spun at 100% CPU with
   no output; compiling it to bytecode took half a second.

   What the unit needs is a level 0 function whose loop body holds a
   match with a SHARED handler (so that a static catch survives
   simplification), and a template application, which is what routes the
   unit through a static program at all.  The application's result
   carries nothing compile-time, so the unit emits no macro object and
   its run-time part is translated with the parent's counter still at
   zero -- the same place the static program's counter starts, which is
   what puts the two numberings on top of each other.  The bug report's
   own case reached the same overlap from a mixed module with a macro
   through an alias of a template application's projection; that shape
   decides only WHERE the two ranges fall.  Which numbers coincide is a
   detail; that the compilation terminates is not. *)

let unescape (s : string) : string =
  let n = String.length s in
  let b = Buffer.create n in
  let i = ref 1 in
  (* Three copies of the scan in one loop body, so that the body holds
     three handlers rather than one: the more numbers the term uses
     inside the loop, the less the collision depends on the exact
     arithmetic of two counters. *)
  while !i < n - 1 do
    (match s.[!i] with
     | '\\' ->
       if !i + 1 >= n - 1 then raise Exit;
       (match s.[!i + 1] with
        | '"' -> Buffer.add_char b '"'
        | '\\' -> Buffer.add_char b '\\'
        | 'n' -> Buffer.add_char b '\n'
        | 't' -> Buffer.add_char b '\t'
        | 'r' -> Buffer.add_char b '\r'
        | 'b' -> Buffer.add_char b '\b'
        | _ -> raise Exit);
       i := !i + 2
     | c -> Buffer.add_char b c; incr i);
    if !i < n - 1 then
      (match s.[!i] with
       | '\\' ->
         if !i + 1 >= n - 1 then raise Exit;
         (match s.[!i + 1] with
          | '"' -> Buffer.add_char b '"'
          | '\\' -> Buffer.add_char b '\\'
          | 'n' -> Buffer.add_char b '\n'
          | 't' -> Buffer.add_char b '\t'
          | 'r' -> Buffer.add_char b '\r'
          | 'b' -> Buffer.add_char b '\b'
          | _ -> raise Exit);
         i := !i + 2
       | c -> Buffer.add_char b c; incr i);
    if !i < n - 1 then
      (match s.[!i] with
       | '\\' ->
         if !i + 1 >= n - 1 then raise Exit;
         (match s.[!i + 1] with
          | '"' -> Buffer.add_char b '"'
          | '\\' -> Buffer.add_char b '\\'
          | 'n' -> Buffer.add_char b '\n'
          | 't' -> Buffer.add_char b '\t'
          | 'r' -> Buffer.add_char b '\r'
          | 'b' -> Buffer.add_char b '\b'
          | _ -> raise Exit);
         i := !i + 2
       | c -> Buffer.add_char b c; incr i)
  done;
  Buffer.contents b

module V = struct let n = 21 end
module G = Static_program_exit_numbers_lib.F[V]

let () = Printf.printf "%s|%d\n" (unescape "\"a\\nb\"") G.v
