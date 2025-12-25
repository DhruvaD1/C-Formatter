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

let show_help () =
  print_endline "Usage: c-formatter [options] [file]";
  print_endline "Options:";
  print_endline "  -h, --help    Show this help";
  print_endline "  -o <file>     Output to file";
  print_endline "";
  print_endline "Examples:";
  print_endline "  c-formatter file.c";
  print_endline "  c-formatter -o output.c input.c";
  print_endline "  cat file.c | c-formatter"

let () =
  let args = Array.to_list Sys.argv in
  match args with
  | [_] ->
    let content = read_stdin () in
    let formatted = format_c_code content in
    print_string formatted
  | [_; "-h"] | [_; "--help"] ->
    show_help ()
  | [_; filename] ->
    let content = read_file filename in
    let formatted = format_c_code content in
    print_string formatted
  | [_; "-o"; output_file; input_file] ->
    let content = read_file input_file in
    let formatted = format_c_code content in
    let oc = open_out output_file in
    output_string oc formatted;
    close_out oc
  | _ ->
    Printf.eprintf "Error: Invalid arguments\n";
    show_help ();
    exit 1
