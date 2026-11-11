type call = { start : string; duration : string; pid : string; program : string; dir : string;
              exit : string; args : string list; outputs : string list;
              rusage : (string * string) list }

module Sexp = struct
  type t = Atom of string | List of t list
end

module Csexp = Csexp.Make (Sexp)

let rusage_fields =
  [ "user_cpu_time"; "system_cpu_time"; "maxrss"; "minflt"; "majflt"; "inblock"; "oublock";
    "nvcsw"; "nivcsw" ]

let field name fields =
  List.find_map (function Sexp.List (Atom n :: args) when n = name -> Some args | _ -> None) fields

let atom name fields = match field name fields with Some [ Atom a ] -> a | _ -> ""

let atoms name fields =
  match field name fields with
  | Some [ List l ] -> List.filter_map (function Sexp.Atom a -> Some a | List _ -> None) l
  | _ -> []

let call : Sexp.t -> call option = function
  | List (Atom "process" :: Atom "finish" :: List [ Atom start; Atom duration ] :: fields) ->
      let rusage =
        match field "rusage" fields with
        | Some [ List usage ] -> List.map (fun name -> (name, atom name usage)) rusage_fields
        | _ -> List.map (fun name -> (name, "")) rusage_fields
      in
      Some { start; duration; pid = atom "pid" fields; program = atom "prog" fields;
             dir = atom "dir" fields; exit = atom "exit" fields;
             args = atoms "process_args" fields; outputs = atoms "target_files" fields; rusage }
  | _ -> None

let read path =
  match In_channel.with_open_bin path Csexp.input_many with
  | Ok sexps -> List.filter_map call sexps
  | Error e -> failwith (path ^ ": " ^ e)

let record table { start; duration; pid; program; dir; exit; args; outputs; rusage } =
  Table.append table
    ~header:([ "start"; "duration"; "pid"; "program"; "dir"; "exit"; "args"; "outputs" ]
             @ rusage_fields)
    ([ start; duration; pid; program; dir; exit; String.concat " " args;
       String.concat " " outputs ] @ List.map snd rusage)

let () =
  match Array.to_list Sys.argv with
  | _ :: out :: args ->
      let context, files = Table.of_args args in
      let table = Table.configure ~out context in
      List.iter (fun path -> List.iter (record table) (read path)) files
  | _ -> prerr_endline "usage: trace OUT.csv COLUMN=VALUE ... -- TRACE ..."; exit 2
