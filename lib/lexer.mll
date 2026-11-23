{
  open Parser
  exception Lexing_error of string
}

rule read = parse
  | [' ' '\t' '\r' '\n']     { read lexbuf }       (* skip whitespace *)
  | ['0'-'9']+ as i          { match int_of_string_opt i with
                               | Some n when n <= 2147483647 -> INT n
                               | _ -> raise (Lexing_error ("integer literal out of range: " ^ i)) }
  | '+'                      { PLUS }
  | '-'                      { MINUS }
  | '*'                      { TIMES }
  | '/'                      { DIV }
  | '%'                      { MOD }
  | '('                      { LPAREN }
  | ')'                      { RPAREN }

  | "true"                   { TRUE }
  | "false"                  { FALSE }
  | "&&"                     { AND }
  | "||"                     { OR }
  | "not"                   { NOT }

  | "if"                     { IF }
  | "then"                   { THEN }
  | "else"                   { ELSE }

  | "=="                     { EQEQ }
  | "!="                     { NE }
  | "="                      { EQ }
  | "<"                      { LT }
  | ">"                      { GT }
  | "<="                     { LE }
  | ">="                     { GE }
  | "<>"                     { NE }

  | "let"                    { LET }
  | "in"                     { IN }

  | "fun"                    { FUN }
  | "->"                     { ARROW }

  | "int"                    { TINT }
  | "bool"                   { TBOOL }
  | ":"                      { COLON }

  | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { ID id }

  | eof                      { EOF }
  | _ as c                   { raise (Lexing_error (Printf.sprintf "Unexpected char: %c" c)) }
