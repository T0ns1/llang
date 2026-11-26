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
  assert_equal (UnOp (Not, UnOp (Not, Bool true))) (parse_string "not not true")

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
      "(if true then 1 else 2) * 3"; "not (1 < 2)"; "- -3"; "not not true";
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
       ]

let () = run_test_tt_main suite
