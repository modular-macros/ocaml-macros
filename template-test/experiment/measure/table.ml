type t = { out : string; context : (string * string) list }

let configure ~out context = { out; context }

let append { out; context } ~header row =
  let columns, values = List.split context in
  let fresh = not (Sys.file_exists out) in
  let csv = Csv.to_channel (open_out_gen [ Open_append; Open_creat; Open_text ] 0o644 out) in
  if fresh then Csv.output_record csv (columns @ header);
  Csv.output_record csv (values @ row);
  Csv.close_out csv

let of_args args =
  let pair arg =
    match String.index_opt arg '=' with
    | Some i -> (String.sub arg 0 i, String.sub arg (i + 1) (String.length arg - i - 1))
    | None -> failwith ("not COLUMN=VALUE: " ^ arg)
  in
  let rec split context = function
    | "--" :: files -> (List.rev context, files)
    | arg :: rest -> split (pair arg :: context) rest
    | [] -> (List.rev context, [])
  in
  split [] args
