type indent_style = 
  | Spaces of int
  | Tabs

type brace_style =
  | Allman
  | KernighanRitchie
  | Stroustrup

type format_config = {
  indent_style: indent_style;
  brace_style: brace_style;
  max_line_length: int;
  preserve_comments: bool;
  compact_empty_functions: bool;
}

let default_config = {
  indent_style = Spaces 4;
  brace_style = KernighanRitchie;
  max_line_length = 80;
  preserve_comments = true;
  compact_empty_functions = false;
}

let create_config ?(indent_spaces=4) ?(brace_style=KernighanRitchie) 
                  ?(max_line_length=80) ?(preserve_comments=true) 
                  ?(compact_empty_functions=false) () =
  {
    indent_style = Spaces indent_spaces;
    brace_style = brace_style;
    max_line_length = max_line_length;
    preserve_comments = preserve_comments;
    compact_empty_functions = compact_empty_functions;
  }
