module Ident =
struct 
  type t =
      Local of { name: string; stamp: int }
    | Scoped of { name: string; stamp: int; scope: int }
    | Global of string
    | Predef of { name: string; stamp: int }

  (* From typing/ident.ml *)
  let compare x y =
    match x, y with
    | Local x, Local y ->
        let c = x.stamp - y.stamp in
        if c <> 0 then c
        else compare x.name y.name
    | Local _, _ -> 1
    | _, Local _ -> (-1)
    | Scoped x, Scoped y ->
        let c = x.stamp - y.stamp in
        if c <> 0 then c
        else compare x.name y.name
    | Scoped _, _ -> 1
    | _, Scoped _ -> (-1)
    | Global x, Global y -> compare x y
    | Global _, _ -> 1
    | _, Global _ -> (-1)
    | Predef { stamp = s1; _ }, Predef { stamp = s2; _ } -> compare s1 s2
end