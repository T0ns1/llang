(* The program after closure conversion: no fun inside expressions.
   Every fun is hoisted into a top-level [fundef], and replaced where it
   appeared by a [MakeClosure] that captures its free variables. *)

open Symbols

type cexp =
  | Num of int
  | Bool of bool
  | Var of symbol * Ast.l_type          (* a parameter, a let-bound or a captured variable *)
  | BinOp of Ast.bin_op * cexp * cexp * Ast.l_type   (* never App: see Apply *)
  | UnOp of Ast.un_op * cexp * Ast.l_type
  | If of cexp * cexp * cexp * Ast.l_type
  | Let of symbol * cexp * cexp * Ast.l_type
  | MakeClosure of int * (symbol * Ast.l_type) list * Ast.l_type
      (* hoisted function id, variables to capture, the closure's type *)
  | Apply of cexp * cexp * Ast.l_type   (* call a closure with an argument *)

type fundef = {
  id : int;                             (* f_0, f_1, ... *)
  param : symbol * Ast.l_type;
  env : (symbol * Ast.l_type) list;     (* the free variables, in layout order *)
  body : cexp;
  ret : Ast.l_type;                     (* = the type of body *)
}

type program = { funs : fundef list; main : cexp }

let type_of = function
  | Num _ -> Ast.TInt
  | Bool _ -> Ast.TBool
  | Var (_, t) | BinOp (_, _, _, t) | UnOp (_, _, t) | If (_, _, _, t)
  | Let (_, _, _, t) | MakeClosure (_, _, t) | Apply (_, _, t) -> t

(* ---------- Printing (llang --cc) ---------- *)

let string_of_type = Parser_utils.string_of_type

let string_of_var (x, t) = string_of_symbol x ^ " : " ^ string_of_type t

(* Fully parenthesised, so the structure is unambiguous. *)
let rec string_of_cexp = function
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Var (x, _) -> string_of_symbol x
  | BinOp (op, a, b, _) ->
      "(" ^ string_of_cexp a ^ " " ^ Parser_utils.unparse_bin_op op ^ " " ^ string_of_cexp b ^ ")"
  | UnOp (op, a, _) -> "(" ^ Parser_utils.unparse_un_op op ^ string_of_cexp a ^ ")"
  | If (c, a, b, _) ->
      "(if " ^ string_of_cexp c ^ " then " ^ string_of_cexp a ^ " else " ^ string_of_cexp b ^ ")"
  | Let (x, a, b, _) ->
      "(let " ^ string_of_symbol x ^ " = " ^ string_of_cexp a ^ " in " ^ string_of_cexp b ^ ")"
  | MakeClosure (id, env, _) ->
      "closure(f_" ^ string_of_int id ^ ", ["
      ^ String.concat ", " (List.map (fun (x, _) -> string_of_symbol x) env) ^ "])"
  | Apply (f, a, _) -> "(" ^ string_of_cexp f ^ " " ^ string_of_cexp a ^ ")"

let string_of_fundef f =
  "f_" ^ string_of_int f.id
  ^ "(env {" ^ String.concat ", " (List.map string_of_var f.env) ^ "}, "
  ^ string_of_var f.param ^ ") : " ^ string_of_type f.ret ^ " =\n  "
  ^ string_of_cexp f.body ^ "\n"

let string_of_program p =
  String.concat "" (List.map string_of_fundef p.funs)
  ^ "main =\n  " ^ string_of_cexp p.main ^ "\n"
