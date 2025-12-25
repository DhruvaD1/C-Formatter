open Lexer

let format_tokens tokens =
  let rec aux tokens acc indent prev_token =
    match tokens with
    | [] -> String.concat "" (List.rev acc)
    | EOF :: _ -> String.concat "" (List.rev acc)
    | Newline :: rest -> aux rest acc indent (Some Newline)
    | Space _ :: rest -> aux rest acc indent prev_token
    | Preprocessor prep :: rest ->
      let new_acc = prep :: "\n" :: acc in
      aux rest new_acc indent None
    | LineComment comment :: rest ->
      let new_acc = "//" :: comment :: "\n" :: acc in
      aux rest new_acc indent None
    | BlockComment comment :: rest ->
      let new_acc = " /*" :: comment :: "*/ " :: acc in
      aux rest new_acc indent (Some (BlockComment comment))
    | LBrace :: rest ->
      let space = match prev_token with
        | Some RParen -> " "
        | Some (Identifier _) -> " "
        | _ -> ""
      in
      let new_acc = space :: "{\n" :: (String.make (indent + 4) ' ') :: acc in
      aux rest new_acc (indent + 4) (Some LBrace)
    | RBrace :: rest ->
      let new_indent = max 0 (indent - 4) in
      let new_acc = "\n" :: (String.make new_indent ' ') :: "}\n" :: (String.make new_indent ' ') :: acc in
      aux rest new_acc new_indent (Some RBrace)
    | LParen :: rest ->
      let space = match prev_token with
        | Some (Keyword "if") | Some (Keyword "while") | Some (Keyword "for") | Some (Keyword "switch") -> " "
        | _ -> ""
      in
      aux rest (space :: "(" :: acc) indent (Some LParen)
    | RParen :: rest ->
      aux rest (")" :: acc) indent (Some RParen)
    | LBracket :: rest ->
      aux rest ("[" :: acc) indent (Some LBracket)
    | RBracket :: rest ->
      aux rest ("]" :: acc) indent (Some RBracket)
    | Semicolon :: rest ->
      aux rest (";\n" :: (String.make indent ' ') :: acc) indent (Some Semicolon)
    | Comma :: rest ->
      aux rest (", " :: acc) indent (Some Comma)
    | Dot :: rest ->
      aux rest ("." :: acc) indent (Some Dot)
    | Question :: rest ->
      aux rest (" ? " :: acc) indent (Some Question)
    | Colon :: rest ->
      aux rest (" : " :: acc) indent (Some Colon)
    | Keyword kw :: rest ->
      let space_before = match prev_token with
        | None | Some Newline | Some LBrace | Some Semicolon -> ""
        | _ -> " "
      in
      aux rest (kw :: space_before :: acc) indent (Some (Keyword kw))
    | Identifier id :: rest ->
      let space_before = match prev_token with
        | None | Some Newline | Some LBrace | Some Semicolon -> ""
        | Some LParen | Some Comma -> ""
        | Some (Keyword _) -> " "
        | _ -> " "
      in
      aux rest (id :: space_before :: acc) indent (Some (Identifier id))
    | Number num :: rest ->
      let space_before = match prev_token with
        | None | Some Newline | Some LBrace | Some Semicolon -> ""
        | Some LParen | Some Comma -> ""
        | Some (Operator "=") -> " "
        | _ -> " "
      in
      aux rest (num :: space_before :: acc) indent (Some (Number num))
    | String str :: rest ->
      let space_before = match prev_token with
        | None | Some Newline | Some LBrace | Some Semicolon -> ""
        | Some LParen | Some Comma -> ""
        | _ -> " "
      in
      aux rest ("\"" :: str :: "\"" :: space_before :: acc) indent (Some (String str))
    | Char ch :: rest ->
      let space_before = match prev_token with
        | None | Some Newline | Some LBrace | Some Semicolon -> ""
        | Some LParen | Some Comma -> ""
        | _ -> " "
      in
      aux rest ("'" :: ch :: "'" :: space_before :: acc) indent (Some (Char ch))
    | Operator op :: rest ->
      match op with
      | "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "==" | "!=" | "<" | ">" | "<=" | ">=" 
      | "+" | "-" | "*" | "/" | "%" | "&&" | "||" | "<<" | ">>" | "&" | "|" | "^" ->
        aux rest (" " :: op :: " " :: acc) indent (Some (Operator op))
      | "++" | "--" ->
        aux rest (op :: acc) indent (Some (Operator op))
      | "!" | "~" ->
        let space_before = match prev_token with
          | None | Some Newline | Some LBrace | Some Semicolon | Some LParen | Some Comma -> ""
          | _ -> " "
        in
        aux rest (op :: space_before :: acc) indent (Some (Operator op))
      | "->" ->
        aux rest (op :: acc) indent (Some (Operator op))
      | _ ->
        aux rest (" " :: op :: " " :: acc) indent (Some (Operator op))
  in
  aux tokens [] 0 None

let format_c_code input =
  let tokens = tokenize input in
  let formatted = format_tokens tokens in
  let lines = String.split_on_char '\n' formatted in
  let cleaned_lines = List.map (fun line -> 
    let trimmed = String.trim line in
    if trimmed = "" then "" else trimmed
  ) lines in
  let non_empty_lines = List.filter (fun line -> line <> "") cleaned_lines in
  String.concat "\n" non_empty_lines ^ "\n"
