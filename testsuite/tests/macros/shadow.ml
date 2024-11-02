(* TEST
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run;
   check-program-output;
*)

macro mkfun k = << fun (x : int) -> $(k <<x>>) >>

let f1 = $(mkfun @@ fun x ->
           mkfun @@ fun y ->
           << ($x, $y)>> )

let () = assert (f1 3 4 = (3, 4))


macro mklet v k = << let x = $v in $(k <<x>>) >> 

let f2 = $(mklet <<2>> @@ fun x ->
           mklet <<3>> @@ fun y ->
           << ($x, $y) >>)

let () = assert (f2 = (2, 3))

macro mkmutlet v k = << let x = ref $v in $(k << !x >>) >> 

let f3 = $(mkmutlet <<20>> @@ fun x ->
           mkmutlet <<30>> @@ fun y ->
           << ($x, $y) >>)

let () = assert (f3 = (20, 30))

macro mkletrec v k =
   << let rec f x = if x = $v then true else g (x - 1)
          and g x = if x = $v then false else f (x-1) in
        $(k <<f>> <<g>>) >>

let f4 = $(mkletrec <<0>> @@ fun evenp1 _oddp2 ->
           mkletrec <<1>> @@ fun _oddp2 evenp2 ->
           << ($evenp1 10, $evenp2 10) >>)

let () = assert (f4 = (true, true))

macro mktrywith e k = << try raise $e with ex -> $(k <<ex>>) >>

let f5 = $(mktrywith <<Division_by_zero>> @@ fun d ->
           mktrywith <<Not_found>>        @@ fun n ->
           << ($d, $n) >>)

let () = assert (f5 = (Division_by_zero, Not_found))

macro mkfor lo hi k = << for i = $lo to $hi do $(k <<i>>) done >>

let f6 = $(<< let r = ref 0 in
           $(mkfor <<1>> <<3>> @@ fun i ->
             mkfor <<10>> <<12>> @@ fun j ->
             << r := !r + $i + $j >>);
             !r >>)

let () = assert (f6 = 117)
