open C_formatter.Formatter

let read_file filename =
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  content

let read_stdin () =
  let buffer = Buffer.create 1024 in
  let rec loop () =
    try
      let line = input_line stdin in
      Buffer.add_string buffer line;
      Buffer.add_char buffer '\n';
      loop ()
    with End_of_file -> Buffer.contents buffer
  in
  loop ()

let () =
  let input_content = 
    if Array.length Sys.argv > 1 then
      read_file Sys.argv.(1)
    else
      read_stdin ()
  in
  let formatted = format_c_code input_content in
  print_string formatted
