type t = { path : string; modname : string; bytes : int }

let read path =
  In_channel.with_open_bin path (fun ic ->
      let magic = really_input_string ic (String.length Config.cmo_magic_number) in
      if magic <> Config.cmo_magic_number then failwith (path ^ ": not a .cmo of this compiler");
      seek_in ic (input_binary_int ic);
      let cu = (input_value ic : Cmo_format.compilation_unit) in
      let (Compunit modname) = cu.cu_name in
      { path; modname; bytes = cu.cu_codesize })

let record table { path; modname; bytes } =
  Table.append table ~header:[ "path"; "module"; "bytecode" ] [ path; modname; string_of_int bytes ]

let () =
  match Array.to_list Sys.argv with
  | _ :: out :: args ->
      let context, files = Table.of_args args in
      let table = Table.configure ~out context in
      List.iter (fun path -> record table (read path)) files
  | _ -> prerr_endline "usage: cmo OUT.csv COLUMN=VALUE ... -- FILE.cmo ..."; exit 2
