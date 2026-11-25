%{
open Ast
open Symbols

(* To see parser errors, run with: ocamlyacc -v -b /tmp/parser_debug parser.mly so that files are generated to /tmp *)

%}

%token <int> INT
%token PLUS MINUS TIMES DIV MOD LPAREN RPAREN EOF
%token TRUE FALSE AND OR NOT 
%token EQ NE LT LE GT GE
%token IF THEN ELSE
%token <string> ID
%token LET ASSIGN IN
%token FUN ARROW
%token TINT TBOOL COLON

%start main
%type <Ast.ast> main

%%
main:
  cond EOF                  { $1 }

cond:
  | disj                         { $1 }
  | LET ID ASSIGN cond IN cond { Let ((new_symbol $2), $4, $6) }
  | IF cond THEN cond ELSE cond  { If ($2, $4, $6) } 

disj:
  | disj OR conj                 { BinOp (Or, $1, $3) }
  | conj                         { $1 }

conj:
  | conj AND comp          { BinOp (And, $1, $3) }
  | comp                    { $1 }

comp:
  | expr EQ  expr           { BinOp (Eq, $1, $3) }
  | expr NE  expr           { BinOp (Ne, $1, $3) }
  | expr LT  expr           { BinOp (Lt, $1, $3) }
  | expr LE  expr           { BinOp (Le, $1, $3) }
  | expr GT  expr           { BinOp (Gt, $1, $3) }
  | expr GE  expr           { BinOp (Ge, $1, $3) }
  | expr                    { $1 }

expr:
  | expr PLUS  term         { BinOp (Add, $1, $3) }
  | expr MINUS term         { BinOp (Sub, $1, $3) }
  | term                    { $1 }

term:
  | term TIMES call         { BinOp (Mul, $1, $3) }
  | term DIV   call         { BinOp (Div, $1, $3) }
  | term MOD   call         { BinOp (Mod, $1, $3) }
  | call                    { $1 }

call: 
  | MINUS call            { UnOp (Neg, $2) }
  | NOT call               { UnOp (Not, $2) }
  | factor                  { $1 }

factor:
  | INT                     { Num $1 }
  | ID                      { Id (new_symbol $1) }
  | TRUE                    { Bool true }
  | FALSE                   { Bool false }
  | LPAREN cond RPAREN      { $2 }

