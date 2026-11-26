open Ast
open Ast_typed
open Environment

let type_bin_op op t1 t2 =
  match op, t1, t2 with
  | (Add | Sub | Mul | Div | Mod), TInt, TInt -> TInt
  | (And | Or), TBool, TBool -> TBool
  | (Eq | Ne), TInt, TInt -> TBool
  | (Eq | Ne), TBool, TBool -> TBool
  | (Lt | Le | Gt | Ge), TInt, TInt -> TBool
  | _ ->
      let expected = match op with
        | Add | Sub | Mul | Div | Mod | Lt | Le | Gt | Ge -> "int and int"
        | And | Or -> "bool and bool"
        | Eq | Ne -> "two ints or two bools"
      in
      failwith (Printf.sprintf "%s expects %s, found %s and %s"
        (Parser_utils.unparse_bin_op op) expected
        (Parser_utils.string_of_btype t1) (Parser_utils.string_of_btype t2))

let type_un_op op t =
  match op, t with
  | Neg, TInt -> TInt
  | Not, TBool -> TBool
  | _ ->
      let expected = match op with Neg -> "int" | Not -> "bool" in
      failwith (Printf.sprintf "%s expects %s, found %s"
        (String.trim (Parser_utils.unparse_un_op op)) expected
        (Parser_utils.string_of_btype t))

let rec type_synth env = function
  | Ast.Num n -> Ast_typed.Num n

  | Ast.Bool b -> Ast_typed.Bool b

  | Ast.Id sym -> 
    let t = lookup_opt sym env in
    (match t with
    | Some t -> Ast_typed.Id (sym, t)
    | None -> failwith ("Unbound identifier: " ^ (Symbols.string_of_symbol sym)))

  | Ast.BinOp (op, e1, e2) -> 
      let et1 = type_synth env e1 in 
      let et2 = type_synth env e2 in 
      Ast_typed.BinOp (op, et1, et2, type_bin_op op (type_of et1) (type_of et2))

  | Ast.UnOp (op, e) -> let e = type_synth env e in Ast_typed.UnOp (op, e, type_un_op op (type_of e))

  | Ast.Let (sym, e1, e2) -> 
      let et1 = type_synth env e1 in
      let new_env = add_binding sym (type_of et1) (begin_scope env) in 
      let et2 = type_synth new_env e2 in 
      Ast_typed.Let (sym, et1, et2, type_of et2)

  | Ast.If (e1, e2, e3) -> 
      let et1 = type_synth env e1 in 
      let et2 = type_synth env e2 in 
      let et3 = type_synth env e3 in
      if type_of et1 <> TBool then failwith ("if condition must be bool, found " ^ Parser_utils.string_of_btype (type_of et1));
      if type_of et2 <> type_of et3 then failwith ("if branches have different types: " ^ Parser_utils.string_of_btype (type_of et2) ^ " and " ^ Parser_utils.string_of_btype (type_of et3));
      Ast_typed.If (et1, et2, et3, type_of et2)

let type_synth ast = type_synth empty ast (* overrides the previous definition *)