open Compiler_llvm

(* How to transform LLVM to *)

let unparse_result = function 
  | IConst (x, _) -> string_of_int x
  | Register x -> string_of_register x

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

let unparse_llvm_i = function 
  | Addi32 (r,p1,p2) -> string_of_register r^" = add nsw i32 "^unparse_result p1^", "^unparse_result p2
  | Subi32 (r,p1,p2) -> string_of_register r^" = sub nsw i32 "^unparse_result p1^", "^unparse_result p2
  | Muli32 (r,p1,p2) -> string_of_register r^" = mul nsw i32 "^unparse_result p1^", "^unparse_result p2
  | Divi32 (r,p1,p2) -> string_of_register r^" = sdiv i32 "^unparse_result p1^", "^unparse_result p2
  | Modi32 (r, p1, p2) -> string_of_register r^" = srem i32 "^unparse_result p1^", "^unparse_result p2
  | Cmp (r, op, p1, p2, t) ->
    string_of_register r^" = icmp "^string_of_op op^" "^string_of_llvm_type t ^" "^unparse_result p1^", "^unparse_result p2
  | Br l -> "br label %"^string_of_label l
  | BrI1 (r,l1,l2) -> "br i1 "^unparse_result r^", label %"^string_of_label l1^", label %"^string_of_label l2
  | Phi (r, t, lst) -> 
    let lst_str =
      List.map (fun (r,l) -> "[ "^unparse_result r ^", %"^string_of_label l ^" ]") lst
      |> String.concat ", "
    in
    string_of_register r^" = phi "^string_of_llvm_type t ^" "^lst_str

let preamble = 
  "; ModuleID = 'llang.c'\n"^
  "source_filename = \"llang.c\"\n"^
  "@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\", align 1\n\n"^
  "@.str.s = private unnamed_addr constant [4 x i8] c\"%s\\0A\\00\", align 1\n\n"^
  "@.true = private unnamed_addr constant [5 x i8] c\"true\\00\", align 1\n\n"^
  "@.false = private unnamed_addr constant [6 x i8] c\"false\\00\", align 1\n\n"^
  "define i32 @main() {\n"

let call_printf fmt arg =
  let final = new_reg () in
  "  "^string_of_register final^" = call i32 (ptr, ...) @printf(ptr noundef "^fmt^", "^arg^")\n"

let epilogue ret t =
  let print_code =
    match t with
    | I32 -> call_printf "@.str" ("i32 noundef "^unparse_result ret)
    | I1 ->
      let s = new_reg () in
      "  "^string_of_register s^" = select i1 "^unparse_result ret^", ptr @.true, ptr @.false\n"^
      call_printf "@.str.s" ("ptr noundef "^string_of_register s)
  in
  print_code^
  "  ret i32 0\n"^
  "}\n\n"^
  "declare i32 @printf(ptr noundef, ...)\n"

let string_of_program ast = 
  let ret, label, instrs, blocks = compile_program 0 [] ast in
  let blocks_text = 
    List.fold_left (fun acc (l, b) -> 
      acc^string_of_label l^":\n"^
      (List.fold_left (fun acc i -> acc^"  "^unparse_llvm_i i^"\n") "" b)
    ) "" (blocks@[(label, instrs)])
  in
  preamble ^blocks_text^ epilogue ret (Ast_typed.type_of ast |> llvm_type_of)


let write_llvm_text_file ast filename =
  let text = string_of_program ast in
  let oc = open_out filename in
  output_string oc text;
  close_out oc
