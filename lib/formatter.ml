open Lexer

let format_tokens tokens =
  let rec aux tokens acc indent in_statement =
    match tokens with
    | [] -> String.concat "" (List.rev acc)
    | EOF :: _ -> String.concat "" (List.rev acc)
    | Newline :: rest -> aux rest acc indent in_statement
    | Space _ :: rest -> aux rest acc indent in_statement
    | LBrace :: rest ->
      let new_acc = 
        if in_statement then " {\n" :: (String.make (indent + 4) ' ') :: acc
        else "{\n" :: (String.make (indent + 4) ' ') :: acc
      in
      aux rest new_acc (indent + 4) false
    | RBrace :: rest ->
      let new_indent = max 0 (indent - 4) in
      let new_acc = "\n" :: (String.make new_indent ' ') :: "}\n" :: (String.make new_indent ' ') :: acc in
      aux rest new_acc new_indent false
    | LParen :: rest ->
      aux rest ("(" :: acc) indent true
    | RParen :: rest ->
      aux rest (")" :: acc) indent in_statement
    | LBracket :: rest ->
      aux rest ("[" :: acc) indent in_statement
    | RBracket :: rest ->
      aux rest ("]" :: acc) indent in_statement
    | Semicolon :: rest ->
      aux rest (";\n" :: (String.make indent ' ') :: acc) indent false
    | Comma :: rest ->
      aux rest (", " :: acc) indent in_statement
    | Dot :: rest ->
      aux rest ("." :: acc) indent in_statement
    | Keyword kw :: rest ->
      let new_acc = 
        if in_statement then " " :: kw :: acc
        else kw :: acc
      in
      aux rest new_acc indent true
    | Identifier id :: rest ->
      let new_acc = 
        if in_statement then " " :: id :: acc
        else id :: acc
      in
      aux rest new_acc indent true
    | Number num :: rest ->
      let new_acc = 
        if in_statement then " " :: num :: acc
        else num :: acc
      in
      aux rest new_acc indent in_statement
    | String str :: rest ->
      let new_acc = 
        if in_statement then " \"" :: str :: "\"" :: acc
        else "\"" :: str :: "\"" :: acc
      in
      aux rest new_acc indent in_statement
    | Char ch :: rest ->
      let new_acc = 
        if in_statement then " '" :: ch :: "'" :: acc
        else "'" :: ch :: "'" :: acc
      in
      aux rest new_acc indent in_statement
    | Operator op :: rest ->
      match op with
      | "=" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "+" | "-" | "*" | "/" | "%" | "&&" | "||" ->
        aux rest (" " :: op :: " " :: acc) indent in_statement
      | "++" | "--" ->
        aux rest (op :: acc) indent in_statement
      | _ ->
        aux rest (" " :: op :: " " :: acc) indent in_statement
  in
  aux tokens [] 0 false

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
