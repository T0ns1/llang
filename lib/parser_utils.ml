(* This module contains the unparser functions for the language *)

open Symbols
open Ast

(* Parser utilities *)

let parse_lexbuf lb =
  try Parser.main Lexer.read lb with
  | Parsing.Parse_error ->
      let pos = lb.Lexing.lex_curr_p in
      let col = pos.pos_cnum - pos.pos_bol in
      failwith (Printf.sprintf "Parse error at line %d, column %d" pos.pos_lnum col)
  | Lexer.Lexing_error msg ->
      let pos = lb.Lexing.lex_curr_p in
      let col = pos.pos_cnum - pos.pos_bol in
      failwith (Printf.sprintf "Lexing error at line %d, column %d: %s" pos.pos_lnum col msg)

let parse_string s =
  Lexing.from_string s |> parse_lexbuf

(* Unparser functions *)

let unparse_bin_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | And -> "&&"
  | Or -> "||"
  | Eq -> "=="
  | Ne -> "!="
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="

let unparse_un_op = function
  | Neg -> "-"
  | Not -> "not "

let rec string_of_btype = function
  | TInt -> "int"
  | TBool -> "bool"
  | TArrow (t1, t2) ->
    let left = match t1 with
      | TArrow _ -> "(" ^ string_of_btype t1 ^ ")"
      | _ -> string_of_btype t1
    in
    left ^ " -> " ^ string_of_btype t2


let rec unparse_ast = function
  | Num x -> string_of_int x

  | Bool b -> string_of_bool b

  | Id sym -> string_of_symbol sym

  | BinOp (op, e1, e2) -> ("(" ^ unparse_ast e1 ^ " " ^ unparse_bin_op op ^ " " ^ unparse_ast e2 ^ ")")

  | UnOp (op, e1) -> ("(" ^ unparse_un_op op ^ unparse_ast e1 ^ ")")

  | Let (sym, e1, e2) -> ("(let " ^ string_of_symbol sym ^ " = " ^ unparse_ast e1 ^ " in " ^ unparse_ast e2 ^ ")")

  | If (e1, e2, e3) -> ("(if " ^ unparse_ast e1 ^ " then " ^ unparse_ast e2 ^ " else " ^ unparse_ast e3 ^ ")")

  | Fun (sym, t, e) -> ("(fun (" ^ string_of_symbol sym ^ " : " ^ string_of_btype t ^ ") -> " ^ unparse_ast e ^ ")")

  | App (e1, e2) -> ("(" ^ unparse_ast e1 ^ " " ^ unparse_ast e2 ^ ")")
