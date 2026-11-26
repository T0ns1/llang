(* This module contains the typed abstract syntax tree for the language *)

open Symbols
open Ast

type ast_typed = 
  | Num of int
  | Bool of bool
  | Id of symbol * l_type
  | Let of symbol * ast_typed * ast_typed * l_type
  | If of ast_typed * ast_typed * ast_typed * l_type
  | BinOp of Ast.bin_op * ast_typed * ast_typed * l_type
  | UnOp of Ast.un_op * ast_typed * l_type

let type_of = function
  | Num _ -> TInt
  | Bool _ -> TBool
  | Id (_, t) -> t
  | Let (_, _, _, t) -> t
  | If (_, _, _, t) -> t
  | BinOp (_, _, _, t) -> t
  | UnOp (_, _, t) -> t





