open Ast
open Symbols
open Environment
open Parser_utils

type value = 
  | Int of int
  | Bool of bool
  | Closure of symbol * ast * value env

let value_from_int n = Int n
let value_from_bool b = Bool b

let int_from_value = function 
  | Int n -> n
  | _ -> failwith "Expected an integer value"

let bool_from_value = function
  | Bool b -> b
  | _ -> failwith "Expected a boolean value"

let string_of_value = function
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Closure (id,e,_) -> "fun "^string_of_symbol id^" -> "^unparse_ast e


let native_eval_un_op op v =
  match op with
  | Neg -> value_from_int (- (int_from_value v))
  | Not -> value_from_bool (not (bool_from_value v))

let rec eval env = function
  | Num n -> value_from_int n

  | Bool b -> value_from_bool b

  | Id id -> lookup id env

  | BinOp (And, e1, e2) -> 
    let v1 = eval env e1 in
    if not (bool_from_value v1) then v1 else eval env e2

  | BinOp (Or, e1, e2) -> 
    let v1 = eval env e1 in
    if bool_from_value v1 then v1 else eval env e2

  | BinOp (op, e1, e2) -> 
    let v1 = eval env e1 in
    let v2 = eval env e2 in
    native_eval_bin_op op v1 v2

  | UnOp (op, e) -> native_eval_un_op op (eval env e)

  | Let (id, e1, e2) -> 
      let v1 = eval env e1 in
      let new_env = add_binding id v1 (begin_scope env) in
      eval new_env e2

  | If (e1, e2, e3) -> 
    let v1 = eval env e1 in
    if bool_from_value v1 then eval env e2 else eval env e3
  
and native_eval_bin_op op v1 v2 =
  match op with
  | Add -> value_from_int (int_from_value v1 + int_from_value v2)
  | Sub -> value_from_int (int_from_value v1 - int_from_value v2)
  | Mul -> value_from_int (int_from_value v1 * int_from_value v2)
  | Div -> value_from_int (int_from_value v1 / int_from_value v2)
  | Mod -> value_from_int (int_from_value v1 mod int_from_value v2)
  | And -> assert false (* Short-circuiting handled in eval *)
  | Or -> assert false (* Short-circuiting handled in eval *)
  | Eq -> value_from_bool (v1 = v2)
  | Ne -> value_from_bool (v1 <> v2)
  | (Lt|Le|Gt|Ge) -> begin match v1, v2 with
    | Int n1, Int n2 -> (match op with
      | Lt -> value_from_bool (n1 < n2)
      | Le -> value_from_bool (n1 <= n2)
      | Gt -> value_from_bool (n1 > n2)
      | Ge -> value_from_bool (n1 >= n2)
      | _ -> assert false)
    | Bool b1, Bool b2 -> (match op with
      | Lt -> value_from_bool (b1 < b2)
      | Le -> value_from_bool (b1 <= b2)
      | Gt -> value_from_bool (b1 > b2)
      | Ge -> value_from_bool (b1 >= b2)
      | _ -> assert false)
    | _ -> failwith "Unsupported binary operator for non matching types"
  end

let eval = eval Environment.empty
