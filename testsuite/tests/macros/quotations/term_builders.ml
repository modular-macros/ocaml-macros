(* TEST
 {
   bytecode;
 }{
   native;
 }
*)

(* The builders of CamlinternalLam make terms whose size is not known
   when a quotation is written.  A code value is the pair of a term and
   its free variables, as Expr builds it. *)

(* An integer switch from a list of cases *)

let name : int -> string =
  $(let unquote : 'a. 'a expr ->
        CamlinternalLam.lam * CamlinternalQuote.Identifier.set_of_t =
      fun e -> Obj.magic e
    in
    let switch i ~numconsts cases fail : string expr =
      let (arg, fvs) = unquote i in
      let fvs = ref fvs in
      let term e =
        let (l, f) = unquote e in
        fvs := CamlinternalQuote.Identifier.merge_free_vars f !fvs;
        l
      in
      let cases = List.map (fun (k, e) -> (k, term e)) cases in
      let fail = Option.map term fail in
      Obj.magic (CamlinternalLam.switch arg ~numconsts cases fail, !fvs)
    in
    << fun c ->
       if c < 3 then
         (* Complete: every integer in the range has a case. *)
         $(switch << c >> ~numconsts:3
             [ (2, << "two" >>); (0, << "zero" >>); (1, << "one" >>) ]
             None)
       else
         (* Incomplete: the cases and the failure branch use the
            argument of the switch, a variable of the quotation. *)
         $(switch << c >> ~numconsts:256
             [ (Char.code 'a', << "letter a" >>);
               (Char.code 'z', << "letter z" >>);
               (Char.code '0', << "digit " ^ string_of_int (c - 48) >>) ]
             (Some << "other " ^ string_of_int c >>)) >>)

(* A switch on a character whose classes are sets of intervals.  The
   body of a class is built once however many characters the class
   has, classes with equal bodies share one, a class whose body is that
   of the failure branch falls through to it, and the body of a class
   of one character stays in line. *)

let kind : char -> string =
  $(let unquote : 'a. 'a expr ->
        CamlinternalLam.lam * CamlinternalQuote.Identifier.set_of_t =
      fun e -> Obj.magic e
    in
    let classify c groups fail : string expr =
      let (arg, fvs) = unquote << Char.code $c >> in
      let fvs = ref fvs in
      let term e =
        let (l, f) = unquote e in
        fvs := CamlinternalQuote.Identifier.merge_free_vars f !fvs;
        l
      in
      let groups =
        List.map
          (fun (intervals, body) ->
             (List.map (fun (lo, hi) -> (Char.code lo, Char.code hi))
                intervals,
              term body))
          groups
      in
      let fail = term fail in
      Obj.magic
        (CamlinternalLam.switch_intervals arg ~numconsts:256 groups
           (Some fail),
         !fvs)
    in
    << fun c ->
       $(let other = << "other " ^ String.make 1 c >> in
         classify << c >>
           [ ([ ('a', 'z'); ('A', 'Z') ],
              << let f k = String.make k c in f 3 ^ " letter" >>);
             ([ ('0', '9') ],
              << "digit " ^ string_of_int (Char.code c - 48) >>);
             ([ (' ', ' ') ], << "space" >>);
             ([ ('\t', '\t'); ('\n', '\n') ], << "space" >>);
             ([ ('_', '_') ], << "underscore" >>);
             ([ ('!', '/') ], other) ]
           other) >>)

(* A switch on a string.  Strings with equal bodies share one, and a
   string whose body is that of the default falls through to it. *)

let arity, none =
  $(let unquote : 'a. 'a expr ->
        CamlinternalLam.lam * CamlinternalQuote.Identifier.set_of_t =
      fun e -> Obj.magic e
    in
    let dispatch : type a. string expr -> (string * a expr) list -> a expr
                        -> a expr =
      fun s cases default ->
        let (arg, fvs) = unquote s in
        let fvs = ref fvs in
        let term e =
          let (l, f) = unquote e in
          fvs := CamlinternalQuote.Identifier.merge_free_vars f !fvs;
          l
        in
        let cases = List.map (fun (name, body) -> (name, term body)) cases in
        let default = term default in
        Obj.magic (CamlinternalLam.string_switch arg cases default, !fvs)
    in
    << (fun s ->
          $(let unknown = << - String.length s >> in
            let two = << let twice f = f (f 0) in twice succ >> in
            dispatch << s >>
              [ ("Leaf", << 0 >>); ("Node", two); ("Pair", two);
                ("A_rather_long_constructor_name", << 3 >>);
                ("Hole", unknown) ]
              unknown)),
       (* Every case is that of the default: nothing is compared. *)
       (fun s ->
          $(let same = << s ^ "!" >> in
            dispatch << s >> [ ("a", same); ("b", same) ] same)) >>)

let () =
  print_endline (String.concat " " (List.init 3 name));
  print_endline
    (String.concat ", "
       (List.map name [ Char.code 'a'; Char.code 'z'; Char.code '0';
                        Char.code 'q' ]));
  print_endline
    (String.concat ", "
       (List.map kind [ 'q'; 'Q'; '7'; ' '; '\n'; '_'; '!'; '~' ]));
  print_endline
    (String.concat " "
       (List.map (fun s -> string_of_int (arity s))
          [ "Leaf"; "Node"; "Pair"; "A_rather_long_constructor_name";
            "Hole"; "Other"; "" ]));
  print_endline (none "a" ^ " " ^ none "z")
