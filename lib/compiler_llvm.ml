(* This module compiles the closure-converted program (Closure_ir) to
   basic blocks of LLVM instructions *)

open Closure_ir
open Environment

type register = int

type label = int

let string_of_register n = "%"^string_of_int n

let string_of_label n = "label_"^string_of_int n

type i_type = 
  | I32
  | I1
  | Ptr

type cmp_op = 
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge

type result = 
  | IConst of int * i_type
  | Register of register
  | Named of string     (* a function parameter: %env, %arg *)
  | Global of string    (* a global: @f_0 *)
  | Null                (* the null pointer *)

type llvm = 
  | Addi32 of register * result * result 
  | Subi32 of register * result * result 
  | Muli32 of register * result * result 
  | Divi32 of register * result * result 
  | Modi32 of register * result * result
  | Cmp of register * cmp_op * result * result * i_type
  | BrI1 of result * label * label
  | Br of label
  | Phi of register * i_type * (result * label) list
  | Malloc of register * int                        (* %r = call ptr @malloc(i64 bytes) *)
  | Gep of register * i_type list * result * int    (* %r = address of field i of a struct *)
  | Store of i_type * result * result               (* store value at address *)
  | Load of register * i_type * result              (* %r = load from address *)
  | Call of register * i_type * result * result * i_type * result
      (* %r = call ret_type fn(ptr env, arg_type arg) *)
  | Ret of i_type * result

let llvm_type_of = function
  | Ast.TInt -> I32
  | Ast.TBool -> I1
  | Ast.TArrow _ -> Ptr   (* every function value is a pointer to a closure record *)

let count = ref 0
let new_reg = fun () -> count := !count + 1; !count

let label_count = ref 0

let new_label = fun () -> label_count := !label_count + 1; !label_count


let empty_basic_block = []

let build_op op ret r1 r2 t = 
  match op, t with 
  | Ast.Add, I32 -> Addi32 (ret,r1,r2)
  | Ast.Sub, I32 -> Subi32 (ret,r1,r2)
  | Ast.Mul, I32 -> Muli32 (ret,r1,r2)
  | Ast.Div, I32 -> Divi32 (ret,r1,r2) 
  | Ast.Mod, I32 -> Modi32 (ret,r1,r2)
  | Ast.Eq, _ -> Cmp (ret,Eq,r1,r2,t)
  | Ast.Ne, _ -> Cmp (ret,Ne,r1,r2,t)
  | Ast.Lt, _ -> Cmp (ret,Lt,r1,r2,t)
  | Ast.Le, _ -> Cmp (ret,Le,r1,r2,t)
  | Ast.Gt, _ -> Cmp (ret,Gt,r1,r2,t)
  | Ast.Ge, _ -> Cmp (ret,Ge,r1,r2,t)
  | _ -> failwith "Compile error: unsupported binary operator"


(* A closure record: { ptr code, ptr env }. *)
let closure_record = [ Ptr; Ptr ]

let fun_name id = "f_" ^ string_of_int id

(* [load_field fields base i t]: the instructions that read field [i], of
   type [t], of the struct [{ fields }] at address [base], and the register
   that holds the value. *)
let load_field fields base i t =
  let addr = new_reg () in
  let v = new_reg () in
  (Register v, [ Gep (addr, fields, base, i); Load (v, t, Register addr) ])

let rec compile_program env l0 b0 = function 
  | Num x -> IConst (x,I32), l0, b0, []

  | Bool b -> IConst ((if b then 1 else 0), I1), l0, b0, []

  | BinOp (op, e1, e2, t) -> compile_binop env l0 b0 e1 e2 t op

  | UnOp (op, e, t) ->
    let r1, l1, b1, bs1 = compile_program env l0 b0 e in
    let ret = new_reg() in
    let instr =
      match op with
      | Ast.Neg -> build_op Mul ret r1 (IConst (-1, I32)) I32   (* -x  =  x * -1 *)
      | Ast.Not -> Cmp (ret, Eq, r1, IConst (0, I1), I1)       (* not x  =  (x == false) *)
    in
    ignore t;
    (Register ret, l1, b1@[instr], bs1)

  | If (cond, then_branch, else_branch, t) ->
      let r1, l1, b1, bs1 = compile_program env l0 b0 cond in
      let then_label = new_label() in
      let r2, l2, b2, bs2 = compile_program env then_label empty_basic_block then_branch in
      let else_label = new_label() in
      let r3, l3, b3, bs3 = compile_program env else_label empty_basic_block else_branch in
      let end_label = new_label() in
      let bs = 
        bs1@
        [(l1, b1@[BrI1 (r1, then_label, else_label)])]@
        bs2@
        [(then_label, b2@[Br end_label])]@
        bs3@
        [(else_label, b3@[Br end_label])]
      in
      let ret = new_reg() in
      (Register ret, end_label, [Phi (ret, llvm_type_of t, [(r2, l2); (r3, l3)])], bs)

  | Let(id,e1,e2,_) -> 
      let r1, l1, b1, bs1 = compile_program env l0 b0 e1 in
      let new_env = env |> begin_scope |> add_binding id r1 in
      let r2, l2, b2, bs2 = compile_program new_env l1 b1 e2 in
      (r2, l2, b2, bs1@bs2 )

  | Var (id, _) -> (lookup id env, l0, b0, [])

  | MakeClosure (id, vars, _) ->
      (* The environment: a heap struct holding the current values of the
         captured variables, in the order closure conversion gave them.
         A function that captures nothing gets a null environment. *)
      let field_types = List.map (fun (_, t) -> llvm_type_of t) vars in
      let env_ptr, env_code =
        if vars = [] then Null, []
        else
          let r = new_reg () in
          let stores =
            List.concat
              (List.mapi
                 (fun i (x, t) ->
                   let addr = new_reg () in
                   [ Gep (addr, field_types, Register r, i);
                     Store (llvm_type_of t, lookup x env, Register addr) ])
                 vars)
          in
          (Register r, Malloc (r, 8 * List.length vars) :: stores)
      in
      (* The closure record: { code pointer, environment pointer }. *)
      let clo = new_reg () in
      let code_addr = new_reg () in
      let env_addr = new_reg () in
      let record_code =
        [ Malloc (clo, 16);
          Gep (code_addr, closure_record, Register clo, 0);
          Store (Ptr, Global (fun_name id), Register code_addr);
          Gep (env_addr, closure_record, Register clo, 1);
          Store (Ptr, env_ptr, Register env_addr) ]
      in
      (Register clo, l0, b0 @ env_code @ record_code, [])

  | Apply (f, arg, t) ->
      (* Load the code and environment out of the closure record and call
         the code with the environment as its first argument. *)
      let r1, l1, b1, bs1 = compile_program env l0 b0 f in
      let r2, l2, b2, bs2 = compile_program env l1 b1 arg in
      let code, load_code = load_field closure_record r1 0 Ptr in
      let env_ptr, load_env = load_field closure_record r1 1 Ptr in
      let ret = new_reg () in
      let instrs =
        load_code @ load_env
        @ [ Call (ret, llvm_type_of t, code, env_ptr, llvm_type_of (type_of arg), r2) ]
      in
      (Register ret, l2, b2 @ instrs, bs1 @ bs2)



and 
  compile_binop env l0 b0 e1 e2 t = function
      | Ast.And ->
          let r1, l1, b1, bs1 = compile_program env l0 b0 e1 in
          let label_right = new_label() in
          let r2, l2, b2, bs2 = compile_program env label_right [] e2 in
          let label_phi = new_label() in
          let branch_left = BrI1 (r1, label_right, label_phi) in
          let exit_block_left = (l1, b1 @ [branch_left]) in
          let branch_right = Br label_phi in
          let right_exit_block = (l2, b2 @ [branch_right]) in
          let register_phi = new_reg() in
          let merge_instruction = Phi (register_phi, I1, [(r1, l1);(r2, l2)]) in
          (Register register_phi, label_phi, [merge_instruction], bs1 @ [exit_block_left] @ bs2 @ [right_exit_block])

      | Ast.Or ->
          let r1, l1, b1, bs1 = compile_program env l0 b0 e1 in
          let label_right = new_label() in
          let r2, l2, b2, bs2 = compile_program env label_right [] e2 in
          let label_phi = new_label() in
          let branch_left = BrI1 (r1, label_phi, label_right) in
          let exit_block_left = (l1, b1 @ [branch_left]) in
          let branch_right = Br label_phi in
          let right_exit_block = (l2, b2 @ [branch_right]) in
          let register_phi = new_reg() in
          let merge_instruction = Phi (register_phi, I1, [(r1, l1);(r2, l2)]) in
          (Register register_phi, label_phi, [merge_instruction], bs1 @ [exit_block_left] @ bs2 @ [right_exit_block])

      | op -> 
          let r1,l1,b1,bs1 = compile_program env l0 b0 e1 in
          let r2,l2,b2,bs2 = compile_program env l1 b1 e2 in
          let ret = new_reg() in
          (* operand type, not result type: a comparison of ints is icmp i32 *)
          ignore t;
          (Register ret, l2, b2@[build_op op ret r1 r2 (llvm_type_of (type_of e1))], bs1@bs2)

(* [compile_fun f]: the basic blocks of hoisted function [f]. It starts
   from an empty environment: only the parameter (%arg) and the captured
   variables, loaded from %env in the entry block, are in scope. *)
let compile_fun (f : fundef) =
  let field_types = List.map (fun (_, t) -> llvm_type_of t) f.env in
  let env0 = empty |> begin_scope |> add_binding (fst f.param) (Named "arg") in
  let env, prologue =
    List.fold_left
      (fun (env, code) (i, (x, t)) ->
        let v, load = load_field field_types (Named "env") i (llvm_type_of t) in
        (add_binding x v env, code @ load))
      (env0, [])
      (List.mapi (fun i v -> (i, v)) f.env)
  in
  let entry = new_label () in
  let r, l, b, bs = compile_program env entry prologue f.body in
  bs @ [ (l, b @ [ Ret (llvm_type_of f.ret, r) ]) ]

let compile_program:(label -> llvm list -> Closure_ir.cexp -> result * label * llvm list * (label * llvm list) list) = compile_program empty
(* overrides the previous declaration*)
