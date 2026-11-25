(* bin/main.ml: command-line entry point for llang *)

open Llang.Parser_utils
open Llang.Interpreter
open Llang.Typing

(* ---------- Reading the program ---------- *)

let read_file file =
  In_channel.with_open_text file In_channel.input_all

let read_stdin () =
  In_channel.input_all stdin

(* ---------- Pipelines ---------- *)

let interpret src =
  let ast = parse_string src in
  let _typed = type_synth ast in (* Reject non well-typed expressions *)
  print_endline (string_of_value (eval ast))

let compile src =
  let ast = parse_string src in
  let typed = type_synth ast in
  print_string (Llang.Llvm_utils.string_of_program typed)

let parse src =
  let ast = parse_string src in
  print_endline (unparse_ast ast)

(* ---------- Errors ---------- *)

(* Exception -> Message mapping *)
let message_of_exception = function
  | Failure msg -> msg
  | Division_by_zero -> "Runtime error: division by zero"
  | Sys_error msg -> "Error: " ^ msg
  | e -> "Unexpected error: " ^ Printexc.to_string e

(* ---------- REPL ---------- *)

let repl () =
  print_endline "Welcome to the L language REPL! (in OCaml)";
  print_endline "Insert an expression. Ctrl+D to exit.";
  let rec loop () =
    print_string "> "; flush stdout;
    match read_line () with
    | line ->
        (try interpret line with e -> prerr_endline (message_of_exception e));
        loop ()
    | exception End_of_file -> print_newline ()
  in
  loop ()

(* ---------- Command line ---------- *)

let usage () =
  prerr_endline "usage: llang -i [file] | llang -c [file] | llang -p [file]";
  exit 2

let () =
  try
    match Array.to_list Sys.argv |> List.tl with
    | ["-i"; file] -> interpret (read_file file)
    | ["-i"]       -> repl ()
    | ["-c"; file] -> compile (read_file file)
    | ["-c"]       -> compile (read_stdin ())
    | ["-p"; file] -> parse (read_file file)
    | ["-p"]       -> parse (read_stdin ())
    | _            -> usage ()
  with e ->
    prerr_endline (message_of_exception e);
    exit 1