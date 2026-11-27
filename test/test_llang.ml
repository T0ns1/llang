open OUnit2
open Llang.Ast
open Llang.Symbols
open Llang.Parser_utils
open Llang.Interpreter

(* Mainly AI generated tests *)

let test_parses_a_single_number _ =
  assert_equal (Num 3) (parse_string "3")

let test_parses_addition _ =
  assert_equal (BinOp (Add, Num 3, Num 4)) (parse_string "3 + 4")

let test_parses_precedence _ =
  assert_equal
    (BinOp (Add, Num 3, BinOp (Mul, Num 4, Num 2)))
    (parse_string "3 + 4 * 2")

let test_parses_parentheses _ =
  assert_equal
    (BinOp (Mul, BinOp (Add, Num 3, Num 4), Num 2))
    (parse_string "(3 + 4) * 2")

let test_parses_unary_minus _ =
  assert_equal (UnOp (Neg, Num 3)) (parse_string "-3")

let test_parses_subtraction_and_division _ =
  assert_equal
    (BinOp (Div, BinOp (Sub, Num 10, Num 4), Num 2))
    (parse_string "(10 - 4) / 2")

(* unparse_ast puts parentheses around every operation, so the printed form
   shows exactly how the parser grouped the program. *)
let test_unparses_back_to_a_readable_expression _ =
  assert_equal ~printer:Fun.id "(3 + (4 * (2 - 1)))" (unparse_ast (parse_string "3 + 4 * (2 - 1)"))

let test_raises_on_invalid_syntax _ =
  assert_raises (Failure "Parse error at line 1, column 3") (fun () ->
      parse_string "3 +")

let test_evaluates_a_single_number _ =
  assert_equal (Int 3) (eval (Num 3))

let test_evaluates_addition _ =
  assert_equal (Int 7) (eval (parse_string "3 + 4"))

let test_evaluates_precedence _ =
  assert_equal (Int 11) (eval (parse_string "3 + 4 * 2"))

let test_evaluates_parentheses _ =
  assert_equal (Int 14) (eval (parse_string "(3 + 4) * 2"))

let test_evaluates_unary_minus _ =
  assert_equal (Int (-3)) (eval (parse_string "-3"))

let test_evaluates_subtraction_and_division _ =
  assert_equal (Int 3) (eval (parse_string "(10 - 4) / 2"))

let test_evaluates_nested_unary_minus _ =
  assert_equal (Int 3) (eval (parse_string "- -3"))

let test_evaluates_a_boolean_literal _ =
  assert_equal (Bool true) (eval (parse_string "true"))

let test_evaluates_logical_and _ =
  assert_equal (Bool false) (eval (parse_string "true && false"))

let test_evaluates_logical_or _ =
  assert_equal (Bool true) (eval (parse_string "false || true"))

let test_evaluates_logical_not _ =
  assert_equal (Bool false) (eval (parse_string "not true"))

let test_evaluates_an_if_expression _ =
  assert_equal (Int 3) (eval (parse_string "if true then 3 else 4"))

let test_parses_equality _ =
  assert_equal (BinOp (Eq, Num 3, Num 4)) (parse_string "3 == 4")

let test_parses_inequality _ =
  assert_equal (BinOp (Ne, Num 3, Num 4)) (parse_string "3 != 4")

let test_parses_less_than _ =
  assert_equal (BinOp (Lt, Num 3, Num 4)) (parse_string "3 < 4")

let test_parses_less_than_or_equal _ =
  assert_equal (BinOp (Le, Num 3, Num 4)) (parse_string "3 <= 4")

let test_parses_greater_than _ =
  assert_equal (BinOp (Gt, Num 3, Num 4)) (parse_string "3 > 4")

let test_parses_greater_than_or_equal _ =
  assert_equal (BinOp (Ge, Num 3, Num 4)) (parse_string "3 >= 4")


let test_evaluates_equality_true _ =
  assert_equal (Bool true) (eval (parse_string "3 == 3"))

let test_evaluates_equality_false _ =
  assert_equal (Bool false) (eval (parse_string "3 == 4"))

let test_evaluates_inequality _ =
  assert_equal (Bool true) (eval (parse_string "3 != 4"))

let test_evaluates_less_than _ =
  assert_equal (Bool true) (eval (parse_string "3 < 4"))

let test_evaluates_less_than_or_equal _ =
  assert_equal (Bool true) (eval (parse_string "4 <= 4"))

let test_evaluates_greater_than _ =
  assert_equal (Bool true) (eval (parse_string "5 > 4"))

let test_evaluates_greater_than_or_equal _ =
  assert_equal (Bool true) (eval (parse_string "4 >= 4"))

let test_evaluates_boolean_comparisons _ =
  assert_equal (Bool true) (eval (parse_string "false < true"))

let test_evaluates_comparison_in_condition _ =
  assert_equal (Int 1) (eval (parse_string "if 3 < 4 then 1 else 2"))

let test_evaluates_a_let_binding _ =
  assert_equal (Int 3) (eval (parse_string "let x = 3 in x"))

let test_evaluates_a_let_binding_used_in_expression _ =
  assert_equal (Int 8) (eval (parse_string "let x = 3 in x + 5"))

let test_evaluates_nested_let_bindings _ =
  assert_equal (Int 7) (eval (parse_string "let x = 3 in let y = 4 in x + y"))

let test_evaluates_shadowed_let_binding _ =
  assert_equal (Int 5) (eval (parse_string "let x = 3 in let x = 5 in x"))

let test_evaluates_let_binding_not_visible_outside_body _ =
  assert_equal (Int 8)
    (eval (parse_string "(let x = 3 in x) + (let x = 5 in x)"))

let test_evaluates_let_bound_value_in_if_condition _ =
  assert_equal (Int 1)
    (eval (parse_string "let x = true in if x then 1 else 2"))

let test_unparses_a_comparison _ =
  assert_equal ~printer:Fun.id "(3 < 4)" (unparse_ast (parse_string "3 < 4"))

let test_unparses_a_let_binding _ =
  assert_equal ~printer:Fun.id "(let x = 3 in x)" (unparse_ast (parse_string "let x = 3 in x"))

(* ---------- Module 2: front end ---------- *)

(* true when f raises Failure with a message starting with prefix *)
let fails_with_prefix prefix f =
  match f () with
  | _ -> false
  | exception Failure msg ->
      String.length msg >= String.length prefix
      && String.sub msg 0 (String.length prefix) = prefix

let assert_parse_error s =
  assert_bool ("expected a parse error for: " ^ s)
    (fails_with_prefix "Parse error" (fun () -> parse_string s))

let x = new_symbol "x"

let test_parses_modulo_like_times _ =
  assert_equal
    (BinOp (Add, Num 1, BinOp (Mod, BinOp (Mul, Num 2, Num 3), Num 4)))
    (parse_string "1 + 2 * 3 % 4")

let test_comparisons_are_non_associative _ =
  assert_parse_error "1 < 2 < 3";
  assert_parse_error "1 == 2 == 3"

let test_comparison_binds_tighter_than_and _ =
  assert_equal
    (BinOp (And, BinOp (Lt, Num 1, Num 2), Bool true))
    (parse_string "1 < 2 && true")

let test_and_binds_tighter_than_or _ =
  assert_equal
    (BinOp (Or, Bool true, BinOp (And, Bool false, Bool true)))
    (parse_string "true || false && true")

let test_not_binds_tighter_than_equality _ =
  assert_equal
    (BinOp (Eq, UnOp (Not, Bool true), Bool false))
    (parse_string "not true == false")

let test_parses_repeated_prefix_operators _ =
  assert_equal (UnOp (Neg, UnOp (Neg, Num 3))) (parse_string "- -3");
  assert_equal (UnOp (Not, UnOp (Not, Bool true))) (parse_string "not (not true)");
  (* not behaves like a function (spec 3.1): not not true would be (not not) true,
     so not only takes an atom and the unparenthesised form is a parse error *)
  assert_raises (Failure "Parse error at line 1, column 7") (fun () -> parse_string "not not true")

let test_not_needs_no_space _ =
  assert_equal (UnOp (Not, Bool true)) (parse_string "not(true)")

let test_keyword_prefixes_are_identifiers _ =
  assert_equal (Id (new_symbol "iffy")) (parse_string "iffy");
  assert_equal (Id (new_symbol "notx")) (parse_string "notx")

let test_parses_a_let _ =
  assert_equal (Let (x, Num 1, BinOp (Add, Id x, Num 1))) (parse_string "let x = 1 in x + 1")

let test_let_body_extends_right _ =
  assert_equal
    (Let (x, Num 1, BinOp (And, BinOp (Gt, Id x, Num 0), BinOp (Lt, Id x, Num 10))))
    (parse_string "let x = 1 in x > 0 && x < 10")

let test_let_in_parentheses _ =
  assert_equal (BinOp (Add, Let (x, Num 1, Id x), Num 1)) (parse_string "(let x = 1 in x) + 1")

let test_let_rhs_can_be_an_if _ =
  assert_equal
    (Let (x, If (Bool true, Num 1, Num 2), Id x))
    (parse_string "let x = if true then 1 else 2 in x")

let test_if_else_branch_extends_right _ =
  assert_equal
    (If (Bool true, Num 1, BinOp (Add, Num 2, Num 3)))
    (parse_string "if true then 1 else 2 + 3")

let test_old_syntax_is_rejected _ =
  assert_parse_error "3 = 4";
  assert_parse_error "3 <> 4"

let test_raises_on_bad_character _ =
  assert_bool "expected a lexing error"
    (fails_with_prefix "Lexing error" (fun () -> parse_string "3 @ 4"))

(* parse (unparse (parse s)) gives the same tree as parse s *)
let test_unparse_round_trips _ =
  List.iter
    (fun s ->
      let tree = parse_string s in
      assert_equal ~msg:s tree (parse_string (unparse_ast tree)))
    [ "(1 + 2) * 3"; "-(3 - 5)"; "1 - (2 - 3)"; "(let x = 1 in x) + 1";
      "(if true then 1 else 2) * 3"; "not (1 < 2)"; "- -3"; "not (not true)";
      "let x = 1 in let y = x in x + y"; "if a then b else c + 1";
      "not true == false"; "1 + 2 * 3 % 4"; "true || false && true" ]

let test_evaluates_modulo _ =
  assert_equal (Int 1) (eval (parse_string "7 % 3"));
  (* truncates toward zero, like LLVM's srem *)
  assert_equal (Int (-1)) (eval (parse_string "-7 % 2"))

let test_types_modulo _ =
  assert_equal TInt (Llang.Ast_typed.type_of (Llang.Typing.type_synth (parse_string "7 % 3")))

(* ---------- Module 3: interpreter and scope ---------- *)

let eval_s s = eval (parse_string s)

let test_and_short_circuits _ =
  assert_equal (Bool false) (eval_s "false && 1 / 0 == 0")

let test_or_short_circuits _ =
  assert_equal (Bool true) (eval_s "true || 1 / 0 == 0")

let test_and_evaluates_right_when_needed _ =
  assert_raises Division_by_zero (fun () -> eval_s "true && 1 / 0 == 0")

let test_or_evaluates_right_when_needed _ =
  assert_raises Division_by_zero (fun () -> eval_s "false || 1 / 0 == 0")

let test_and_or_truth_tables _ =
  List.iter
    (fun (s, b) -> assert_equal ~msg:s (Bool b) (eval_s s))
    [ ("true && true", true); ("true && false", false);
      ("false && true", false); ("false && false", false);
      ("true || true", true); ("true || false", true);
      ("false || true", true); ("false || false", false) ]

let test_if_skips_unused_branch _ =
  assert_equal (Int 1) (eval_s "if true then 1 else 1 / 0");
  assert_equal (Int 2) (eval_s "if false then 1 / 0 else 2")

let test_let_rhs_sees_outer_binding _ =
  (* the warm-up program: y = 2, then x = 20, so x + y = 22 *)
  assert_equal (Int 22)
    (eval_s "let x = 1 in let y = x + 1 in let x = y * 10 in x + y")

let test_if_in_let_in_if _ =
  assert_equal (Int 30)
    (eval_s "if 1 < 2 then let x = 10 in if x > 5 then x * 3 else 0 else 1")

let test_operands_evaluate_left_to_right _ =
  (* both operands fail; the left one must be reported *)
  assert_raises (Failure "Unbound identifier: y") (fun () -> eval_s "y + 1 / 0")

let test_unbound_identifier_fails _ =
  assert_raises (Failure "Unbound identifier: zz") (fun () -> eval_s "zz")

let contains_substring haystack needle =
  let hlen = String.length haystack and nlen = String.length needle in
  let rec go i =
    i + nlen <= hlen && (String.sub haystack i nlen = needle || go (i + 1))
  in
  nlen = 0 || go 0


(* ---------- Module 4: types ---------- *)

let type_s s = Llang.Ast_typed.type_of (Llang.Typing.type_synth (parse_string s))

(* the type checker fails with exactly this message (main.ml adds "Type error: ") *)
let assert_type_error msg s =
  assert_raises ~msg:s (Failure msg) (fun () -> type_s s)

let test_types_warmup_programs _ =
  List.iter
    (fun (s, t) -> assert_equal ~msg:s t (type_s s))
    [ ("let x = 1 in if x < 2 then x else 0", TInt);
      ("if true then 1 else 1 / 0", TInt);
      ("let x = 1 in let x = true in x", TBool);
      ("(if true then 1 else 2) < 3", TBool);
      ("true == (1 < 2)", TBool) ]

let test_let_shadowing_changes_type _ =
  assert_equal TBool (type_s "let x = 1 in let x = x < 2 in x");
  assert_equal TInt (type_s "let x = true in (let x = 1 in x) + (if x then 1 else 0)")

let test_type_errors_have_messages _ =
  assert_type_error "if condition must be bool, found int" "if 1 then 2 else 3";
  assert_type_error "if branches have different types: int and bool" "if true then 1 else false";
  assert_type_error "Unbound identifier: x" "x";
  assert_type_error "Unbound identifier: x" "(let x = 1 in x) + x";
  assert_type_error "+ expects int and int, found bool and int" "let x = true in x + 1";
  assert_type_error "&& expects bool and bool, found bool and int" "true && 1";
  assert_type_error "< expects int and int, found bool and bool" "true < false";
  assert_type_error "== expects two ints or two bools, found int and bool" "1 == true";
  assert_type_error "- expects int, found bool" "-true";
  assert_type_error "not expects bool, found int" "not 1"

let test_type_checker_checks_unused_branch _ =
  (* never evaluated, still rejected: the check is static *)
  assert_type_error "+ expects int and int, found int and bool" "if true then 1 else 1 + true"

(* ---------- Module 5: LLVM ---------- *)

let ir_of s = Llang.Llvm_utils.string_of_program (Llang.Typing.type_synth (parse_string s))

let assert_ir_contains s needle =
  assert_bool (Printf.sprintf "IR for %S should contain %S" s needle) (contains_substring (ir_of s) needle)

let assert_ir_lacks s needle =
  assert_bool (Printf.sprintf "IR for %S should not contain %S" s needle) (not (contains_substring (ir_of s) needle))

let test_ir_if_uses_phi _ =
  assert_ir_contains "if 1 < 2 then 10 else 20" "br i1";
  assert_ir_contains "if 1 < 2 then 10 else 20" "phi i32"

let test_ir_bool_if_uses_i1_phi _ =
  assert_ir_contains "if true then false else true" "phi i1"

let test_ir_and_short_circuits _ =
  (* the right operand must sit behind a conditional branch *)
  assert_ir_contains "false && 1 / 0 == 0" "br i1";
  assert_ir_contains "false && 1 / 0 == 0" "phi i1"

let test_ir_or_short_circuits _ =
  assert_ir_contains "true || 1 / 0 == 0" "br i1";
  assert_ir_contains "true || 1 / 0 == 0" "phi i1"

let test_ir_comparison_uses_operand_type _ =
  (* the bug in L0: comparing ints as i1 *)
  assert_ir_contains "5 == 7" "icmp eq i32";
  assert_ir_contains "true == false" "icmp eq i1"

let test_ir_let_emits_no_instruction _ =
  (* x is just the constant 1: no add, no alloca *)
  assert_ir_lacks "let x = 1 in x" "= add";
  assert_ir_lacks "let x = 1 in x" "alloca"

let test_ir_mod_uses_srem _ =
  assert_ir_contains "7 % 3" "srem i32"

let test_ir_bool_prints_text _ =
  assert_ir_contains "true" "select i1"

let test_ir_has_no_target_triple _ =
  assert_ir_lacks "1" "target triple"

(* ---------- Module 6: functions ---------- *)

let test_string_of_function_types _ =
  let check expected t = assert_equal ~printer:(fun s -> s) expected (string_of_btype t) in
  (* -> is right-associative: only an arrow on the left needs parentheses *)
  check "int -> int -> int" (TArrow (TInt, TArrow (TInt, TInt)));
  check "(int -> int) -> int" (TArrow (TArrow (TInt, TInt), TInt));
  check "(int -> bool) -> (bool -> int) -> int"
    (TArrow (TArrow (TInt, TBool), TArrow (TArrow (TBool, TInt), TInt)));
  check "((int -> int) -> int) -> int" (TArrow (TArrow (TArrow (TInt, TInt), TInt), TInt))

let test_eval_applies_a_function _ =
  assert_equal (Int 42) (eval_s "(fun (x : int) -> x + 1) 41")

let test_eval_scoping_is_static _ =
  (* f sees the x where it was written (1), not the later x = 100 *)
  assert_equal (Int 2)
    (eval_s "let x = 1 in let f = fun (y : int) -> y + x in let x = 100 in f 1");
  (* the spec's example: dynamic scoping would give 6 *)
  assert_equal (Int 5)
    (eval_s "let x = 1 in let f = fun (y : int) -> y + x in let g = fun (x : int) -> x + f x in g 2")

let test_eval_currying _ =
  assert_equal (Int 7) (eval_s "let add = fun (x : int) -> fun (y : int) -> x + y in add 3 4");
  (* partial application keeps x = 5 in the returned closure *)
  assert_equal (Int 15)
    (eval_s "let add = fun (x : int) -> fun (y : int) -> x + y in let add5 = add 5 in add5 10")

let test_eval_higher_order _ =
  assert_equal (Int 12)
    (eval_s "let twice = fun (f : int -> int) -> fun (x : int) -> f (f x) in \
               twice (twice (fun (x : int) -> x + 3)) 0");
  (* a closure chosen by an if, then applied *)
  assert_equal (Int 11)
    (eval_s "let f = if true then fun (x : int) -> x + 1 else fun (x : int) -> x * 2 in f 10")

let test_eval_function_errors _ =
  assert_raises Division_by_zero
    (fun () -> eval_s "let f = fun (d : int) -> 100 / d in f 5 + f 0");
  (* eval on its own fails cleanly on a non-function (the typer normally rejects it) *)
  assert_raises (Failure "Expected a closure") (fun () -> eval_s "3 4")

let test_functions_print_as_fun _ =
  assert_equal ~printer:(fun s -> s) "<fun>" (string_of_value (eval_s "fun (x : int) -> x"));
  assert_equal ~printer:(fun s -> s) "<fun>"
    (string_of_value (eval_s "let k = fun (x : int) -> fun (y : int) -> x in k 1"))

let test_parses_application_and_fun _ =
  let f = new_symbol "f" and g = new_symbol "g" and x = new_symbol "x" in
  (* application is left-associative *)
  assert_equal (App (App (Id f, Id g), Id x)) (parse_string "f g x");
  assert_equal (App (Id f, App (Id g, Id x))) (parse_string "f (g x)");
  (* not behaves like a function: not f x is (not f) x *)
  assert_equal (App (UnOp (Not, Id f), Id x)) (parse_string "not f x");
  (* unary minus is looser than application: - f x is -(f x) *)
  assert_equal (UnOp (Neg, App (Id f, Id x))) (parse_string "- f x");
  (* application binds tighter than * and + *)
  assert_equal (BinOp (Add, App (App (Id f, Num 3), Num 4), Num 1)) (parse_string "f 3 4 + 1");
  (* f -1 is a subtraction, not f applied to -1 *)
  assert_equal (BinOp (Sub, Id f, Num 1)) (parse_string "f -1");
  (* the body of fun extends as far right as possible *)
  assert_equal (Fun (x, TInt, BinOp (Add, Id x, Num 1))) (parse_string "fun (x : int) -> x + 1")

let test_parses_function_types _ =
  let param_type s = match parse_string s with
    | Fun (_, t, _) -> t
    | _ -> assert_failure ("not a fun: " ^ s) in
  (* -> is right-associative *)
  assert_equal (TArrow (TInt, TArrow (TInt, TInt))) (param_type "fun (f : int -> int -> int) -> f");
  assert_equal (TArrow (TArrow (TInt, TInt), TInt)) (param_type "fun (f : (int -> int) -> int) -> f");
  assert_equal (TArrow (TInt, TBool)) (param_type "fun (f : (int -> bool)) -> f")

let test_types_of_functions _ =
  List.iter
    (fun (s, t) -> assert_equal ~msg:s ~printer:string_of_btype t (type_s s))
    [ ("fun (x : int) -> x > 0", TArrow (TInt, TBool));
      ("(fun (x : int) -> x > 0) 1", TBool);
      ("fun (f : int -> int) -> f (f 1)", TArrow (TArrow (TInt, TInt), TInt));
      ("fun (x : int) -> fun (y : int) -> x + y", TArrow (TInt, TArrow (TInt, TInt)));
      ("let add = fun (x : int) -> fun (y : int) -> x + y in add 3", TArrow (TInt, TInt));
      (* both branches have the same function type *)
      ("if true then fun (x : int) -> x else fun (y : int) -> y * 2", TArrow (TInt, TInt));
      (* the parameter shadows an outer x of another type *)
      ("let x = true in fun (x : int) -> x + 1", TArrow (TInt, TInt)) ]

let test_function_type_errors _ =
  assert_type_error "attempting to apply a non-function type: int" "3 4";
  assert_type_error "function argument type mismatch: expected int, found bool"
    "(fun (x : int) -> x) true";
  assert_type_error "function argument type mismatch: expected int -> int, found bool -> int"
    "(fun (f : int -> int) -> f 1) (fun (b : bool) -> 1)";
  (* functions can't be compared *)
  assert_type_error "== expects two ints or two bools, found int -> int and int -> int"
    "let f = fun (x : int) -> x in f == f";
  assert_type_error "if branches have different types: int -> int and int -> bool"
    "if true then fun (x : int) -> x else fun (x : int) -> x > 0";
  (* the parameter is only in scope inside the body *)
  assert_type_error "Unbound identifier: x" "(fun (x : int) -> x) x"

let test_lexer_identifiers_with_underscores _ =
  assert_equal (Id (new_symbol "apply_pos")) (parse_string "apply_pos");
  assert_equal (Id (new_symbol "_x")) (parse_string "_x");
  assert_equal (Int 2) (eval_s "let _x = 1 in _x + 1")

let suite =
  "llang parser suite"
  >::: [
         "parses a single number" >:: test_parses_a_single_number;
         "parses addition" >:: test_parses_addition;
         "parses multiplication before addition (precedence)"
         >:: test_parses_precedence;
         "parses parentheses" >:: test_parses_parentheses;
         "parses unary minus" >:: test_parses_unary_minus;
         "parses subtraction and division"
         >:: test_parses_subtraction_and_division;
         "unparses back to a readable expression"
         >:: test_unparses_back_to_a_readable_expression;
         "raises on invalid syntax" >:: test_raises_on_invalid_syntax;
         "evaluates a single number" >:: test_evaluates_a_single_number;
         "evaluates addition" >:: test_evaluates_addition;
         "evaluates multiplication before addition (precedence)"
         >:: test_evaluates_precedence;
         "evaluates parentheses" >:: test_evaluates_parentheses;
         "evaluates unary minus" >:: test_evaluates_unary_minus;
         "evaluates subtraction and division"
         >:: test_evaluates_subtraction_and_division;
         "evaluates nested unary minus" >:: test_evaluates_nested_unary_minus;
         "evaluates a boolean literal" >:: test_evaluates_a_boolean_literal;
         "evaluates logical and" >:: test_evaluates_logical_and;
         "evaluates logical or" >:: test_evaluates_logical_or;
         "evaluates logical not" >:: test_evaluates_logical_not;
         "evaluates an if expression" >:: test_evaluates_an_if_expression;
         "parses equality" >:: test_parses_equality;
         "parses inequality" >:: test_parses_inequality;
         "parses less than" >:: test_parses_less_than;
         "parses less than or equal" >:: test_parses_less_than_or_equal;
         "parses greater than" >:: test_parses_greater_than;
         "parses greater than or equal" >:: test_parses_greater_than_or_equal;
         "evaluates equality (true)" >:: test_evaluates_equality_true;
         "evaluates equality (false)" >:: test_evaluates_equality_false;
         "evaluates inequality" >:: test_evaluates_inequality;
         "evaluates less than" >:: test_evaluates_less_than;
         "evaluates less than or equal" >:: test_evaluates_less_than_or_equal;
         "evaluates greater than" >:: test_evaluates_greater_than;
         "evaluates greater than or equal"
         >:: test_evaluates_greater_than_or_equal;
         "evaluates boolean comparisons" >:: test_evaluates_boolean_comparisons;
         "evaluates comparison in if condition"
         >:: test_evaluates_comparison_in_condition;
         "evaluates a let binding" >:: test_evaluates_a_let_binding;
         "evaluates a let binding used in expression"
         >:: test_evaluates_a_let_binding_used_in_expression;
         "evaluates nested let bindings" >:: test_evaluates_nested_let_bindings;
         "evaluates shadowed let binding"
         >:: test_evaluates_shadowed_let_binding;
         "evaluates let binding not visible outside body"
         >:: test_evaluates_let_binding_not_visible_outside_body;
         "evaluates let bound value in if condition"
         >:: test_evaluates_let_bound_value_in_if_condition;
         "unparses a comparison" >:: test_unparses_a_comparison;
         "unparses a let binding" >:: test_unparses_a_let_binding;
         "parses % at the level of *" >:: test_parses_modulo_like_times;
         "comparisons are non-associative" >:: test_comparisons_are_non_associative;
         "comparison binds tighter than &&" >:: test_comparison_binds_tighter_than_and;
         "&& binds tighter than ||" >:: test_and_binds_tighter_than_or;
         "not binds tighter than ==" >:: test_not_binds_tighter_than_equality;
         "parses repeated prefix operators" >:: test_parses_repeated_prefix_operators;
         "not needs no space" >:: test_not_needs_no_space;
         "keyword prefixes are identifiers" >:: test_keyword_prefixes_are_identifiers;
         "parses a let" >:: test_parses_a_let;
         "let body extends right" >:: test_let_body_extends_right;
         "let in parentheses" >:: test_let_in_parentheses;
         "let right-hand side can be an if" >:: test_let_rhs_can_be_an_if;
         "if else-branch extends right" >:: test_if_else_branch_extends_right;
         "old = and <> syntax is rejected" >:: test_old_syntax_is_rejected;
         "raises a lexing error on a bad character" >:: test_raises_on_bad_character;
         "unparse round-trips" >:: test_unparse_round_trips;
         "evaluates modulo" >:: test_evaluates_modulo;
         "types modulo" >:: test_types_modulo;
         "&& short-circuits" >:: test_and_short_circuits;
         "|| short-circuits" >:: test_or_short_circuits;
         "&& evaluates the right side when needed" >:: test_and_evaluates_right_when_needed;
         "|| evaluates the right side when needed" >:: test_or_evaluates_right_when_needed;
         "&& and || truth tables" >:: test_and_or_truth_tables;
         "if skips the unused branch" >:: test_if_skips_unused_branch;
         "let right-hand side sees the outer binding" >:: test_let_rhs_sees_outer_binding;
         "if in let in if" >:: test_if_in_let_in_if;
         "operands evaluate left to right" >:: test_operands_evaluate_left_to_right;
         "unbound identifier fails" >:: test_unbound_identifier_fails;
         "types the warm-up programs" >:: test_types_warmup_programs;
         "let shadowing can change the type" >:: test_let_shadowing_changes_type;
         "type errors have clear messages" >:: test_type_errors_have_messages;
         "type checker checks the unused branch" >:: test_type_checker_checks_unused_branch;
         "IR: if uses br i1 and phi" >:: test_ir_if_uses_phi;
         "IR: bool if uses an i1 phi" >:: test_ir_bool_if_uses_i1_phi;
         "IR: && short-circuits" >:: test_ir_and_short_circuits;
         "IR: || short-circuits" >:: test_ir_or_short_circuits;
         "IR: comparison uses the operand type" >:: test_ir_comparison_uses_operand_type;
         "IR: let emits no instruction" >:: test_ir_let_emits_no_instruction;
         "IR: % uses srem" >:: test_ir_mod_uses_srem;
         "IR: bools print as text" >:: test_ir_bool_prints_text;
         "IR: no target triple" >:: test_ir_has_no_target_triple;
         "function types print with minimal parentheses" >:: test_string_of_function_types;
         "eval: applying a function" >:: test_eval_applies_a_function;
         "eval: scoping is static" >:: test_eval_scoping_is_static;
         "eval: currying and partial application" >:: test_eval_currying;
         "eval: higher-order functions" >:: test_eval_higher_order;
         "eval: errors inside and around functions" >:: test_eval_function_errors;
         "eval: functions print as <fun>" >:: test_functions_print_as_fun;
         "parse: application, not, minus and fun" >:: test_parses_application_and_fun;
         "parse: function types" >:: test_parses_function_types;
         "types of functions" >:: test_types_of_functions;
         "function type errors have clear messages" >:: test_function_type_errors;
         "identifiers can contain underscores" >:: test_lexer_identifiers_with_underscores;
       ]

let () = run_test_tt_main suite
