open Compiler_llvm

(* How to transform LLVM to *)

let unparse_result = function 
  | IConst (x, _) -> string_of_int x
  | Register x -> string_of_register x
  | Named s -> "%"^s
  | Global s -> "@"^s
  | Null -> "null"

let string_of_op = function
  | Eq -> "eq"
  | Ne -> "ne"
  | Lt -> "slt"
  | Le -> "sle"
  | Gt -> "sgt"
  | Ge -> "sge"

let string_of_llvm_type = function
  | I32 -> "i32"
  | I1 -> "i1"
  | Ptr -> "ptr"

let string_of_struct ts =
  "{ "^String.concat ", " (List.map string_of_llvm_type ts)^" }"

let unparse_llvm_i = function 
  | Addi32 (r,p1,p2) -> string_of_register r^" = add nsw i32 "^unparse_result p1^", "^unparse_result p2
  | Subi32 (r,p1,p2) -> string_of_register r^" = sub nsw i32 "^unparse_result p1^", "^unparse_result p2
  | Muli32 (r,p1,p2) -> string_of_register r^" = mul nsw i32 "^unparse_result p1^", "^unparse_result p2
  | Divi32 (r,p1,p2) -> string_of_register r^" = sdiv i32 "^unparse_result p1^", "^unparse_result p2
  | Modi32 (r,p1,p2) -> string_of_register r^" = srem i32 "^unparse_result p1^", "^unparse_result p2
  | Cmp (r, op, p1, p2, t) ->
    string_of_register r^" = icmp "^string_of_op op^" "^string_of_llvm_type t ^" "^unparse_result p1^", "^unparse_result p2
  | BrI1 (r,l1,l2) -> "br i1 "^unparse_result r^", label %"^string_of_label l1^", label %"^string_of_label l2
  | Br l -> "br label %"^string_of_label l
  | Phi (r, t, lst) ->
    let lst_str =
      List.map (fun (res, l) -> "[ "^unparse_result res^", %"^string_of_label l^" ]") lst
      |> String.concat ", "
    in
    string_of_register r^" = phi "^string_of_llvm_type t^" "^lst_str
  | Malloc (r, bytes) ->
    string_of_register r^" = call ptr @malloc(i64 "^string_of_int bytes^")"
  | Gep (r, ts, base, i) ->
    string_of_register r^" = getelementptr "^string_of_struct ts^", ptr "^unparse_result base
    ^", i32 0, i32 "^string_of_int i
  | Store (t, v, addr) ->
    "store "^string_of_llvm_type t^" "^unparse_result v^", ptr "^unparse_result addr
  | Load (r, t, addr) ->
    string_of_register r^" = load "^string_of_llvm_type t^", ptr "^unparse_result addr
  | Call (r, ret_t, fn, env, arg_t, arg) ->
    string_of_register r^" = call "^string_of_llvm_type ret_t^" "^unparse_result fn
    ^"(ptr "^unparse_result env^", "^string_of_llvm_type arg_t^" "^unparse_result arg^")"
  | Ret (t, v) -> "ret "^string_of_llvm_type t^" "^unparse_result v

let preamble = 
  "; ModuleID = 'llang.c'\n"^
  "source_filename = \"llang.c\"\n"^
  "target datalayout = \"e-m:o-i64:64-i128:128-n32:64-S128-Fn32\"\n"^
  "target triple = \"arm64-apple-macosx26.0.0\"\n\n"^
  "@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\n"^
  "@.str.s = private unnamed_addr constant [4 x i8] c\"%s\\0A\\00\", align 1\n"^
  "@.true = private unnamed_addr constant [5 x i8] c\"true\\00\", align 1\n"^
  "@.false = private unnamed_addr constant [6 x i8] c\"false\\00\", align 1\n"^
  "@.fun = private unnamed_addr constant [6 x i8] c\"<fun>\\00\", align 1\n\n"

let main_header =
  "; Function Attrs: noinline nounwind optnone ssp uwtable(sync)\n"^
  "define i32 @main() #0 {\n"

(* One printf call: [fmt] is the format global, [arg] the typed argument
   (e.g. "i32 noundef %3" or "ptr noundef @.true"). *)
let call_printf fmt arg =
  let final = new_reg () in
  "  "^string_of_register final^" = call i32 (ptr, ...) @printf(ptr noundef "^fmt^", "^arg^")\n"

(* The printing code is chosen from the program's static type. *)
let epilogue ret t =
  let print_code =
    match t with
    | Ast.TInt -> call_printf "@.str" ("i32 noundef "^unparse_result ret)
    | Ast.TBool ->
        let s = new_reg () in
        "  "^string_of_register s^" = select i1 "^unparse_result ret^", ptr @.true, ptr @.false\n"^
        call_printf "@.str.s" ("ptr noundef "^string_of_register s)
    | Ast.TArrow _ -> call_printf "@.str.s" "ptr noundef @.fun"
  in
  print_code^
  "  ret i32 0\n"^
  "}\n\n"^
  "declare i32 @printf(ptr noundef, ...) #1\n"^
  "declare ptr @malloc(i64)\n"

let string_of_blocks blocks =
  List.fold_left (fun acc (l, b) ->
    acc^string_of_label l^":\n"^
    (List.fold_left (fun acc i -> acc^"  "^unparse_llvm_i i^"\n") "" b)
  ) "" blocks

(* A hoisted function: define <ret> @f_n(ptr %env, <param> %arg). *)
let string_of_fun (f : Closure_ir.fundef) =
  let blocks = compile_fun f in
  "define "^string_of_llvm_type (llvm_type_of f.ret)^" @"^fun_name f.id
  ^"(ptr %env, "^string_of_llvm_type (llvm_type_of (snd f.param))^" %arg) {\n"
  ^string_of_blocks blocks^"}\n\n"

let string_of_program ast =
  let p = Closure_conv.convert_program ast in
  let funs_text = String.concat "" (List.map string_of_fun p.funs) in
  let ret, label, instrs, blocks = compile_program 0 [] p.main in
  preamble^funs_text^main_header^string_of_blocks (blocks@[(label, instrs)])
  ^epilogue ret (Ast_typed.type_of ast)
