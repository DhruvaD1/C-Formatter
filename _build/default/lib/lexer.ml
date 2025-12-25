type token =
  | Keyword of string
  | Identifier of string
  | Number of string
  | String of string
  | Char of string
  | Operator of string
  | LParen | RParen
  | LBrace | RBrace
  | LBracket | RBracket
  | Semicolon | Comma | Dot
  | Question | Colon
  | Preprocessor of string
  | LineComment of string
  | BlockComment of string
  | Newline | Space of int
  | EOF

let keywords = ["int"; "float"; "double"; "char"; "void"; "if"; "else"; "while"; "for"; "return"; "struct"; "union"; "typedef"; "const"; "static"; "extern"; "auto"; "register"; "volatile"; "unsigned"; "signed"; "long"; "short"; "sizeof"; "switch"; "case"; "default"; "break"; "continue"; "do"; "goto"; "enum"]

let is_keyword s = List.mem s keywords

let is_alpha = function 'a'..'z' | 'A'..'Z' | '_' -> true | _ -> false
let is_digit = function '0'..'9' -> true | _ -> false
let is_alnum c = is_alpha c || is_digit c

let rec tokenize input =
  let len = String.length input in
  let rec aux i acc =
    if i >= len then List.rev (EOF :: acc)
    else
      match input.[i] with
      | ' ' | '\t' -> 
        let spaces = count_spaces input i in
        aux (i + spaces) (Space spaces :: acc)
      | '\n' -> aux (i + 1) (Newline :: acc)
      | '(' -> aux (i + 1) (LParen :: acc)
      | ')' -> aux (i + 1) (RParen :: acc)
      | '{' -> aux (i + 1) (LBrace :: acc)
      | '}' -> aux (i + 1) (RBrace :: acc)
      | '[' -> aux (i + 1) (LBracket :: acc)
      | ']' -> aux (i + 1) (RBracket :: acc)
      | ';' -> aux (i + 1) (Semicolon :: acc)
      | ',' -> aux (i + 1) (Comma :: acc)
      | '.' -> aux (i + 1) (Dot :: acc)
      | '?' -> aux (i + 1) (Question :: acc)
      | ':' -> aux (i + 1) (Colon :: acc)
      | '#' ->
        let prep, next_i = read_preprocessor input i in
        aux next_i (Preprocessor prep :: acc)
      | '/' when i + 1 < len && input.[i + 1] = '/' ->
        let comment, next_i = read_line_comment input (i + 2) in
        aux next_i (LineComment comment :: acc)
      | '/' when i + 1 < len && input.[i + 1] = '*' ->
        let comment, next_i = read_block_comment input (i + 2) in
        aux next_i (BlockComment comment :: acc)
      | '"' ->
        let str, next_i = read_string input (i + 1) in
        aux next_i (String str :: acc)
      | '\'' ->
        let char_val, next_i = read_char input (i + 1) in
        aux next_i (Char char_val :: acc)
      | c when is_alpha c ->
        let id, next_i = read_identifier input i in
        let token = if is_keyword id then Keyword id else Identifier id in
        aux next_i (token :: acc)
      | c when is_digit c ->
        let num, next_i = read_number input i in
        aux next_i (Number num :: acc)
      | _ ->
        let op, next_i = read_operator input i in
        aux next_i (Operator op :: acc)
  in
  aux 0 []

and count_spaces input i =
  let rec aux j count =
    if j >= String.length input then count
    else match input.[j] with
      | ' ' | '\t' -> aux (j + 1) (count + 1)
      | _ -> count
  in
  aux i 0

and read_string input i =
  let rec aux j acc =
    if j >= String.length input then (acc, j)
    else match input.[j] with
      | '"' -> (acc, j + 1)
      | '\\' when j + 1 < String.length input -> 
        aux (j + 2) (acc ^ String.make 1 input.[j] ^ String.make 1 input.[j + 1])
      | c -> aux (j + 1) (acc ^ String.make 1 c)
  in
  aux i ""

and read_char input i =
  let rec aux j acc =
    if j >= String.length input then (acc, j)
    else match input.[j] with
      | '\'' -> (acc, j + 1)
      | '\\' when j + 1 < String.length input ->
        aux (j + 2) (acc ^ String.make 1 input.[j] ^ String.make 1 input.[j + 1])
      | c -> aux (j + 1) (acc ^ String.make 1 c)
  in
  aux i ""

and read_identifier input i =
  let rec aux j acc =
    if j >= String.length input then (acc, j)
    else if is_alnum input.[j] then
      aux (j + 1) (acc ^ String.make 1 input.[j])
    else (acc, j)
  in
  aux i ""

and read_number input i =
  let rec aux j acc =
    if j >= String.length input then (acc, j)
    else match input.[j] with
      | c when is_digit c || c = '.' -> aux (j + 1) (acc ^ String.make 1 c)
      | _ -> (acc, j)
  in
  aux i ""

and read_preprocessor input i =
  let rec aux j acc =
    if j >= String.length input then (acc, j)
    else match input.[j] with
      | '\n' -> (acc, j)
      | c -> aux (j + 1) (acc ^ String.make 1 c)
  in
  aux i ""

and read_line_comment input i =
  let rec aux j acc =
    if j >= String.length input then (acc, j)
    else match input.[j] with
      | '\n' -> (acc, j)
      | c -> aux (j + 1) (acc ^ String.make 1 c)
  in
  aux i ""

and read_block_comment input i =
  let rec aux j acc =
    if j + 1 >= String.length input then (acc, j)
    else match (input.[j], input.[j + 1]) with
      | ('*', '/') -> (acc, j + 2)
      | _ -> aux (j + 1) (acc ^ String.make 1 input.[j])
  in
  aux i ""

and read_operator input i =
  if i + 1 < String.length input then
    match (input.[i], input.[i + 1]) with
    | ('=', '=') | ('!', '=') | ('<', '=') | ('>', '=') 
    | ('&', '&') | ('|', '|') | ('+', '+') | ('-', '-')
    | ('<', '<') | ('>', '>') | ('-', '>') | ('+', '=')
    | ('-', '=') | ('*', '=') | ('/', '=') | ('%', '=') -> 
      (String.sub input i 2, i + 2)
    | _ -> (String.make 1 input.[i], i + 1)
  else (String.make 1 input.[i], i + 1)
