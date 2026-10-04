(* The generator of splices_exit_numbers.ml: its code is a function
   with a loop whose body holds matches with shared handlers, numbered
   by the compilation of this unit. *)

$let[..] unescape () = << fun (s : string) ->
  let n = String.length s in
  let b = Buffer.create n in
  let i = ref 1 in
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
       | c -> Buffer.add_char b c; incr i)
  done;
  Buffer.contents b >>
